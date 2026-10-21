#' TSGenerator: Copernicus HR-VPP vegetation time-series processing
#'
#' TSGenerator provides a native-R workflow for acquiring, extracting,
#' quality-assessing, and analysing vegetation time series derived from
#' Copernicus High Resolution Vegetation Phenology and Productivity (HR-VPP)
#' products, including Seasonal Trajectories (ST) and Vegetation Phenology and
#' Productivity (VPP) products.
#'
#' The stable 2.0 API covers WEkEO acquisition, `terra`-based geospatial
#' extraction, product-quality assessment, temporal completeness diagnostics,
#' optional time-series imputation, missingness modelling, visualization, and
#' integrated graphical workflows. Selected TSGenerator 1.x entry points are
#' retained as deprecated compatibility wrappers or migration guards.
#'
#' @section Main workflow:
#' Use [check_wekeo()] and [hda_client()] to validate WEkEO access;
#' [download_st()] or [download_vpp()] for acquisition; [extract_ts()] or
#' [extract_vpp()] for polygon-level geospatial extraction; [quality_info()],
#' [classify_quality()], [summarize_quality()], and [mask_quality()] for product
#' quality; [temporal_plan()], [summarize_missingness()], and
#' [assess_ts_quality()] for temporal diagnostics; [impute_ts()] for optional
#' gap filling; and [model_missingness()] with [plot_missingness()] for
#' missingness analysis. The integrated graphical workflow is launched with
#' [runTSapp()].
#'
#' @keywords internal
"_PACKAGE"
