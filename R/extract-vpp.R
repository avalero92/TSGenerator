# VPP extraction ----------------------------------------------------------

.tsg_vpp_specs <- function() {
  data.frame(
    Product = c("SOSD","EOSD","MAXD","LENGTH","SOSV","EOSV","MINV","MAXV","AMPL","LSLOPE","RSLOPE","SPROD","TPROD","QFLAG"),
    Type = c(rep("date",3),"duration",rep("ppi",5),rep("slope",2),rep("productivity",2),"quality"),
    Unit = c(rep("date",3),"day",rep("PPI",5),rep("PPI day-1",2),rep("PPI day",2),"class"),
    ScaleFactor = c(rep(1,4),rep(10000,7),10,10,1),
    NoData = c(0,0,0,0,rep(-32768,7),65535,65535,NA),
    stringsAsFactors = FALSE
  )
}

.tsg_parse_vpp_names <- function(x) {
  nm <- basename(as.character(x))
  product <- toupper(sub(".*_(SOSD|EOSD|MAXD|LENGTH|SOSV|EOSV|MINV|MAXV|AMPL|LSLOPE|RSLOPE|SPROD|TPROD|QFLAG)(?:\\.tiff?)?$", "\\1", nm, perl=TRUE))
  okp <- grepl("_(SOSD|EOSD|MAXD|LENGTH|SOSV|EOSV|MINV|MAXV|AMPL|LSLOPE|RSLOPE|SPROD|TPROD|QFLAG)(?:\\.tiff?)?$", nm, ignore.case=TRUE, perl=TRUE)
  product[!okp] <- NA_character_
  season <- tolower(sub(".*_(s[12])_(?:SOSD|EOSD|MAXD|LENGTH|SOSV|EOSV|MINV|MAXV|AMPL|LSLOPE|RSLOPE|SPROD|TPROD|QFLAG)(?:\\.tiff?)?$", "\\1", nm, perl=TRUE))
  oks <- grepl("_s[12]_(?:SOSD|EOSD|MAXD|LENGTH|SOSV|EOSV|MINV|MAXV|AMPL|LSLOPE|RSLOPE|SPROD|TPROD|QFLAG)(?:\\.tiff?)?$", nm, ignore.case=TRUE, perl=TRUE)
  season[!oks] <- NA_character_
  year <- suppressWarnings(as.integer(sub("^VPP_([0-9]{4}).*$", "\\1", nm, ignore.case=TRUE, perl=TRUE)))
  oky <- grepl("^VPP_[0-9]{4}_", nm, ignore.case=TRUE)
  year[!oky] <- NA_integer_
  data.frame(Layer = nm, Year = year, Season = season, Product = product, stringsAsFactors=FALSE)
}

.tsg_vpp_meta <- function(x, raster, product=NULL, season=NULL, year=NULL) {
  n <- terra::nlyr(raster)
  candidates <- names(raster)
  if (is.character(x)) {
    files <- tryCatch(.tsg_list_rasters(x), error=function(e) NULL)
    if (!is.null(files) && length(files)==n) candidates <- basename(files)
  }
  meta <- .tsg_parse_vpp_names(candidates)
  recycle <- function(v, name) {
    if (is.null(v)) return(NULL)
    if (length(v)==1L) v <- rep(v,n)
    if (length(v)!=n) stop(sprintf("'%s' must have length 1 or one value per raster layer (%d).", name,n), call.=FALSE)
    v
  }
  p <- recycle(product,"product"); s <- recycle(season,"season"); y <- recycle(year,"year")
  if (!is.null(p)) meta$Product <- toupper(p)
  if (!is.null(s)) meta$Season <- tolower(s)
  if (!is.null(y)) meta$Year <- as.integer(y)
  specs <- .tsg_vpp_specs()
  if (anyNA(meta$Product) || any(!meta$Product %in% specs$Product)) stop("Could not determine a valid VPP product for every layer. Supply 'product' explicitly.", call.=FALSE)
  if (anyNA(meta$Season) || any(!meta$Season %in% c("s1","s2"))) stop("Could not determine season (s1/s2) for every VPP layer. Supply 'season' explicitly.", call.=FALSE)
  if (anyNA(meta$Year) || any(meta$Year < 1900 | meta$Year > 2200)) stop("Could not determine a valid year for every VPP layer. Supply 'year' explicitly.", call.=FALSE)
  meta
}

.tsg_decode_yydoy <- function(v) {
  v <- as.numeric(v)
  out <- rep(NA_real_, length(v))
  good <- is.finite(v) & v > 0
  yy <- floor(v[good] / 1000)
  doy <- round(v[good] %% 1000)
  full_year <- 2000L + as.integer(yy)
  valid <- doy >= 1 & doy <= ifelse((full_year %% 4 == 0 & full_year %% 100 != 0) | full_year %% 400 == 0, 366, 365)
  idx <- which(good)[valid]
  if (length(idx)) out[idx] <- as.numeric(as.Date(sprintf("%04d-01-01", full_year[valid])) + doy[valid] - 1L)
  out
}

.tsg_prepare_vpp_layer <- function(r, spec) {
  nodata <- spec$NoData
  if (!is.na(nodata)) r <- terra::ifel(r == nodata, NA, r)
  if (spec$Type == "date") return(terra::app(r, .tsg_decode_yydoy))
  if (spec$Type %in% c("ppi","slope","productivity")) return(r / spec$ScaleFactor)
  r
}

#' Extract HR-VPP phenology and productivity parameters
#'
#' Extracts polygon-level statistics from Copernicus HR-VPP VPP rasters using
#' product-aware decoding, scaling, NoData handling, season metadata and a
#' terra-first workflow. Date products (SOSD, MAXD, EOSD) are decoded from the
#' YYDOY raster coding before spatial aggregation, preventing invalid averages
#' or medians across encoded calendar values.
#'
#' @param x VPP `terra::SpatRaster`, TIFF path, directory, or vector of TIFFs.
#' @param polygons Polygon `sf`/`sfc` or `terra::SpatVector` object.
#' @param id_col Optional unique polygon ID field.
#' @param product Optional VPP product name(s). Inferred from standard CLMS
#'   filenames when omitted.
#' @param season Optional `s1`/`s2` value(s). Inferred from filenames when omitted.
#' @param year Optional four-digit year value(s). Inferred from filenames when omitted.
#' @param fun Spatial summary: `median` (default), `mean`, `min`, `max`, `sum`,
#'   or a function accepted by `terra::extract()`.
#' @param transform Transform polygons to raster CRS when necessary.
#' @param na.rm Remove missing pixels during spatial aggregation.
#' @param exact Use exact polygon-cell fractions where supported by terra.
#' @param touches Include cells touched by polygon boundaries.
#' @return A `tsg_vpp` data frame with `ID`, `Year`, `Season`, `Product`,
#'   `Unit`, `Layer`, `Value`, and `Date`. Date parameters populate `Date`;
#'   numeric parameters populate `Value`.
#' @family geospatial core
#' @export
extract_vpp <- function(x, polygons, id_col=NULL, product=NULL, season=NULL, year=NULL,
                        fun="median", transform=TRUE, na.rm=TRUE,
                        exact=FALSE, touches=FALSE) {
  .tsg_require_terra()
  plan <- geospatial_plan(x, polygons, id_col=id_col, transform=transform)
  meta <- .tsg_vpp_meta(x, plan$raster, product, season, year)
  specs <- .tsg_vpp_specs()
  summary_fun <- .tsg_match_summary_fun(fun)
  pieces <- vector("list", plan$n_layers)
  for (i in seq_len(plan$n_layers)) {
    spec <- specs[match(meta$Product[i], specs$Product),,drop=FALSE]
    rr <- .tsg_prepare_vpp_layer(plan$raster[[i]], spec)
    z <- terra::extract(rr, plan$polygons, fun=summary_fun, na.rm=na.rm,
                        exact=exact, touches=touches)
    z <- as.data.frame(z)
    vals <- as.numeric(z[[ncol(z)]])
    is_date <- spec$Type == "date"
    pieces[[i]] <- data.frame(
      ID=plan$ids, Year=meta$Year[i], Season=meta$Season[i], Product=meta$Product[i],
      Unit=spec$Unit, Layer=meta$Layer[i],
      Value=if (is_date) NA_real_ else vals,
      Date=as.Date(if (is_date) vals else rep(NA_real_,length(vals)), origin="1970-01-01"),
      stringsAsFactors=FALSE
    )
  }
  out <- do.call(rbind,pieces); rownames(out) <- NULL
  attr(out,"summary_function") <- if (is.character(fun)) tolower(fun) else "custom"
  attr(out,"product_specs") <- specs
  class(out) <- c("tsg_vpp","data.frame")
  out
}

#' @family geospatial core
#' @export
print.tsg_vpp <- function(x, ...) {
  cat("TSGenerator VPP extraction\n")
  cat(sprintf("  Records: %d\n", nrow(x)))
  cat(sprintf("  Features: %d\n", length(unique(x$ID))))
  cat(sprintf("  Products: %s\n", paste(unique(x$Product), collapse=", ")))
  cat(sprintf("  Seasons: %s\n", paste(unique(x$Season), collapse=", ")))
  NextMethod("print", x, ...)
}
