test_that("primary 2.0 API is exported", {
  api <- c(
    "download_st", "download_vpp", "extract_ts", "extract_vpp",
    "quality_info", "classify_quality", "summarize_quality", "mask_quality",
    "temporal_plan", "summarize_missingness", "assess_ts_quality",
    "impute_ts", "model_missingness", "plot_missingness"
  )
  expect_true(all(vapply(api, exists, logical(1), envir = asNamespace("TSGenerator"), inherits = FALSE)))
})

test_that("retired heavy dependencies are not package dependencies", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "TSGenerator"))
  fields <- paste(desc[1, intersect(c("Imports", "Depends", "Suggests"), colnames(desc))], collapse = ",")
  expect_false(grepl("reticulate|raster|plotly|VIM|doParallel", fields))
})
