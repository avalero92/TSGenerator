#' TSGenerator: Copernicus HR-VPP time-series tools
#'
#' TSGenerator provides a native-R workflow for acquiring, extracting, quality
#' controlling, and analysing Copernicus High Resolution Vegetation Phenology
#' and Productivity (HR-VPP) Seasonal Trajectories (ST) and Vegetation
#' Phenology and Productivity (VPP) products.
#'
#' The 2.0 API is organised around four groups: WEkEO acquisition,
#' terra-based geospatial extraction, product-quality handling, and temporal
#' quality/analysis. Selected 1.x entry points remain only as deprecated
#' compatibility wrappers or migration guards.
#'
#' @section Main workflow:
#' Use [check_wekeo()] and [hda_client()] to validate access, [download_st()]
#' or [download_vpp()] for acquisition, [extract_ts()] or [extract_vpp()] for
#' polygon-level extraction, [quality_info()] and related functions for HR-VPP
#' quality flags, and [temporal_plan()] for time-series diagnostics.
#'
#' @importFrom DT datatable
#' @importFrom readr read_csv
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinyjs useShinyjs
#' @keywords internal
"_PACKAGE"
