test_that("primary TSGenerator 2.0 API exists and is exported", {
  api <- c(
    # WEkEO/HDA backend
    "hda_client", "hda_auth_check", "hda_datasets", "hda_query_template",
    "hda_search", "hda_download", "check_wekeo", "check_wekeo_integration",
    # Acquisition and geospatial processing
    "download_st", "download_vpp", "geospatial_plan", "extract_ts", "extract_vpp",
    # Product quality
    "quality_info", "classify_quality", "summarize_quality", "mask_quality",
    # Temporal quality and analysis
    "temporal_plan", "summarize_missingness", "assess_ts_quality",
    "impute_ts", "model_missingness", "plot_missingness",
    # Integrated graphical workflow
    "runTSapp"
  )
  ns <- asNamespace("TSGenerator")
  expect_true(all(vapply(api, exists, logical(1), envir = ns, inherits = FALSE)))
  expect_true(all(api %in% getNamespaceExports("TSGenerator")))
})
