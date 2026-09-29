test_that("primary TSGenerator 2.0 API exists and is exported", {
  api <- c(
    "download_st", "download_vpp", "check_wekeo", "check_wekeo_integration",
    "geospatial_plan", "extract_ts", "extract_vpp", "quality_info",
    "classify_quality", "summarize_quality", "mask_quality", "temporal_plan",
    "summarize_missingness", "assess_ts_quality", "impute_ts",
    "model_missingness", "plot_missingness"
  )
  ns <- asNamespace("TSGenerator")
  expect_true(all(vapply(api, exists, logical(1), envir = ns, inherits = FALSE)))
  expect_true(all(api %in% getNamespaceExports("TSGenerator")))
})
