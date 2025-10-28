#' Apply Quality Flag Mask to Raster Data
#'
#' This function applies a soft mask to raster data based on QFLAG2 quality flag values.
#' It processes raster files in a specified folder, filters pixels based on valid quality
#' flag values, and saves the masked results to an output folder. Pixels with invalid
#' quality flags are set to NA, while valid pixels retain their original values.
#'
#' @param stack_folder Character. Path to the folder containing input raster files.
#'   Must be a valid directory path. Default is NULL (required parameter).
#' @param output_folder Character. Path to the output folder where processed files
#'   will be saved. Directory will be created if it doesn't exist.
#' @param valid_values Numeric vector. QFLAG2 values considered valid for masking.
#'   Default is c(1, 1024, 2048, 4096, 8192), which typically represent different
#'   quality levels in remote sensing data.
#' @param pattern Character. Regular expression pattern to match raster files.
#'   Default is "\\\\.tif$" to match TIFF files.
#' @param qflag_band Integer. Band number containing the QFLAG2 quality information.
#'   Default is 2. Must be a positive integer.
#' @param target_band Integer. Band number containing the data to be masked.
#'   Default is 1. Must be a positive integer.
#' @param use_terra Logical. Whether to use the 'terra' package (TRUE) or 'raster'
#'   package (FALSE) for processing. Default is TRUE. If 'terra' is not available,
#'   automatically falls back to 'raster'.
#' @param verbose Logical. Whether to print processing messages and progress.
#'   Default is TRUE.
#'
#' @return Invisibly returns a named list containing processing statistics:
#' \describe{
#'   \item{total_files}{Total number of files found matching the pattern}
#'   \item{processed}{Number of files successfully processed}
#'   \item{skipped}{Number of files skipped due to insufficient bands}
#'   \item{failed}{Number of files that failed during processing}
#'   \item{output_folder}{Path to the output folder}
#' }
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Validates all input parameters
#'   \item Searches for raster files matching the specified pattern
#'   \item For each file, loads the specified target and quality flag bands
#'   \item Creates a mask based on valid QFLAG2 values
#'   \item Sets pixels with invalid quality flags to NA
#'   \item Saves the masked raster to the output folder with LZW compression
#' }
#'
#' The function supports both 'terra' and 'raster' packages for raster processing.
#' It will attempt to use 'terra' first (if available) as it generally offers
#' better performance, but will fall back to 'raster' if needed.
#'
#' @section QFLAG2 Values:
#' Common QFLAG2 values and their meanings:
#' \itemize{
#'   \item 1: Good quality
#'   \item 1024: Acceptable quality with minor issues
#'   \item 2048: Marginal quality
#'   \item 4096: Poor quality but usable
#'   \item 8192: Fill value or no data
#' }
#'
#' @section Error Handling:
#' The function includes comprehensive error handling:
#' \itemize{
#'   \item Validates all input parameters before processing
#'   \item Checks for package availability and falls back gracefully
#'   \item Skips files with insufficient bands rather than failing
#'   \item Continues processing remaining files if individual files fail
#'   \item Provides detailed warnings and error messages
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' result <- QFLAG2.Mask(
#'   stack_folder = "/path/to/input/rasters",
#'   output_folder = "/path/to/output/folder"
#' )
#'
#' # Custom quality flag values and bands
#' result <- QFLAG2.Mask(
#'   stack_folder = "/path/to/input/rasters",
#'   output_folder = "/path/to/output/folder",
#'   valid_values = c(1, 1024, 2048),
#'   qflag_band = 3,
#'   target_band = 1,
#'   verbose = TRUE
#' )
#'
#' # Process only specific file types
#' result <- QFLAG2.Mask(
#'   stack_folder = "/path/to/input/rasters",
#'   output_folder = "/path/to/output/folder",
#'   pattern = "MOD.*\\.tif$",  # Only MODIS files
#'   use_terra = FALSE          # Force use of raster package
#' )
#'
#' # Check processing results
#' print(result$processed)  # Number of successfully processed files
#' print(result$failed)     # Number of failed files
#' }
#'
#' @note
#' \itemize{
#'   \item Input files must have at least as many bands as the maximum of
#'     qflag_band and target_band
#'   \item Output files are saved with LZW compression and tiling for efficiency
#'   \item The function preserves the original file names in the output folder
#'   \item Processing can be memory-intensive for large raster files
#' }
#'
#' @seealso
#' \code{\link[terra]{rast}}, \code{\link[terra]{writeRaster}} for terra package functions
#' \code{\link[raster]{stack}}, \code{\link[raster]{writeRaster}} for raster package functions
#'
#' @author Alexey Valero Jorge
#'
#' @keywords raster mask quality remote-sensing
#'
#' @export

QFLAG2.Mask <- function(stack_folder = NULL, 
                      output_folder, 
                      valid_values = c(1, 1024, 2048, 4096, 8192),
                      pattern = "\\.tif$",
                      qflag_band = 2,
                      target_band = 1,
                      use_terra = TRUE,
                      verbose = TRUE) {
  
  # ============================================================================
  # Input validation
  # ============================================================================
  
  if (is.null(stack_folder)) {
    stop("Address to raster stack folder is required. Please provide 'stack_folder' parameter.")
  }
  
  if (!dir.exists(stack_folder)) {
    stop("The specified stack_folder does not exist: ", stack_folder)
  }
  
  if (missing(output_folder)) {
    stop("Output folder path is required")
  }
  
  # Validate numeric parameters
  if (!is.numeric(valid_values) || length(valid_values) == 0) {
    stop("valid_values must be a numeric vector with at least one value")
  }
  
  if (!is.numeric(qflag_band) || qflag_band < 1) {
    stop("qflag_band must be a positive integer")
  }
  
  if (!is.numeric(target_band) || target_band < 1) {
    stop("target_band must be a positive integer")
  }
  
  # ============================================================================
  # Setup
  # ============================================================================
  
  # Load appropriate library
  if (use_terra) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      warning("terra package not available, falling back to raster package")
      use_terra <- FALSE
    }
  }
  
  if (!use_terra) {
    if (!requireNamespace("raster", quietly = TRUE)) {
      stop("Neither terra nor raster packages are available")
    }
  }
  
  # Get the list of raster files in the folder
  raster_files <- list.files(path = stack_folder, 
                             pattern = pattern, 
                             full.names = TRUE,
                             ignore.case = TRUE)
  
  if (length(raster_files) == 0) {
    warning("No files found matching pattern '", pattern, "' in folder: ", stack_folder)
    return(invisible(NULL))
  }
  
  # Create output directory if it does not exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    if (verbose) message("Created output directory: ", output_folder)
  }
  
  # ============================================================================
  # Processing statistics
  # ============================================================================
  
  processed_files <- 0
  failed_files <- 0
  skipped_files <- 0
  
  if (verbose) {
    message("Starting processing of ", length(raster_files), " files...")
    message("Using package: ", ifelse(use_terra, "terra", "raster"))
    message("Valid QFLAG2 values: ", paste(valid_values, collapse = ", "))
  }
  
  # ============================================================================
  # Main processing loop
  # ============================================================================
  
  for (i in seq_along(raster_files)) {
    file <- raster_files[i]
    file_name <- basename(file)
    
    if (verbose) {
      message("Processing file ", i, "/", length(raster_files), ": ", file_name)
    }
    
    tryCatch({
      # ========================================================================
      # Load raster using appropriate package
      # ========================================================================
      
      if (use_terra) {
        r <- terra::rast(file)
        n_bands <- terra::nlyr(r)
      } else {
        r <- raster::stack(file)
        n_bands <- raster::nlayers(r)
      }
      
      # ========================================================================
      # Validate raster structure
      # ========================================================================
      
      required_bands <- max(qflag_band, target_band)
      if (n_bands < required_bands) {
        warning("File '", file_name, "' has only ", n_bands, 
                " bands but requires at least ", required_bands, " bands. Skipping.")
        skipped_files <- skipped_files + 1
        next
      }
      
      # ========================================================================
      # Apply soft mask
      # ========================================================================
      
      if (use_terra) {
        # Extract bands
        target_layer <- r[[target_band]]
        qflag_layer <- r[[qflag_band]]
        
        # Create mask: TRUE for pixels to keep, FALSE for pixels to mask
        mask_condition <- terra::values(qflag_layer) %in% valid_values
        
        # Apply mask by setting non-valid pixels to NA
        terra::values(target_layer)[!mask_condition] <- NA
        
        # Create output path
        output_file <- file.path(output_folder, file_name)
        
        # Save the modified file
        terra::writeRaster(target_layer, 
                           filename = output_file, 
                           overwrite = TRUE,
                           gdal = c("COMPRESS=LZW", "TILED=YES"))
        
      } else {
        # Raster package implementation
        target_layer <- r[[target_band]]
        qflag_layer <- r[[qflag_band]]
        
        # Create mask
        mask_condition <- raster::values(qflag_layer) %in% valid_values
        
        # Apply mask
        raster::values(target_layer)[!mask_condition] <- NA
        
        # Create output path
        output_file <- file.path(output_folder, file_name)
        
        # Save the modified file
        raster::writeRaster(target_layer, 
                            filename = output_file, 
                            format = "GTiff", 
                            overwrite = TRUE,
                            options = c("COMPRESS=LZW", "TILED=YES"))
      }
      
      processed_files <- processed_files + 1
      
      if (verbose) {
        message("  ✓ Successfully processed: ", file_name)
      }
      
    }, error = function(e) {
      failed_files <- failed_files + 1
      warning("Failed to process file '", file_name, "': ", e$message)
      
      if (verbose) {
        message("  ✗ Failed: ", file_name, " - ", e$message)
      }
    })
  }
  
  # ============================================================================
  # Summary report
  # ============================================================================
  
  if (verbose) {
    message("\n", rep("=", 50))
    message("PROCESSING SUMMARY")
    message(rep("=", 50))
    message("Total files found: ", length(raster_files))
    message("Successfully processed: ", processed_files)
    message("Skipped files: ", skipped_files)
    message("Failed files: ", failed_files)
    message("Output directory: ", output_folder)
    
    if (failed_files > 0) {
      message("\nNote: Check warnings above for details on failed files.")
    }
  }
  
  # Return invisible summary
  return(invisible(list(
    total_files = length(raster_files),
    processed = processed_files,
    skipped = skipped_files,
    failed = failed_files,
    output_folder = output_folder
  )))
}