# TSGenerator 2.0.0

TSGenerator 2.0 is a major release focused on current Copernicus HR-VPP Seasonal Trajectories (ST) and Vegetation Phenology and Productivity (VPP) workflows. The package has been reorganized around native R acquisition, `terra`-based geospatial processing, explicit product-quality handling, and reproducible time-series analysis.

## Acquisition

- Added native R access to WEkEO HDA through `hdar`; Python and `reticulate` are no longer required for ST/VPP acquisition.
- Added `download_st()` for HR-VPP Seasonal Trajectories, including PPI and ST-QFLAG selection, tile/bounding-box filters, preview mode, structured results, and automatic splitting of long requests into service-compatible temporal windows.
- Added `download_vpp()` with product-aware VPP selection, season handling, spatial/temporal filters, preview mode, and structured results.
- Added `check_wekeo()` and `check_wekeo_integration()` for offline diagnostics and opt-in live integration checks.
- Added bounded retry logic, transfer-size estimates, and skip-existing behaviour for more robust acquisition.
- Retained `Download.STPPI()` and `Download.HRVPP()` as deprecated compatibility wrappers over the 2.0 acquisition API.

## Geospatial processing

- Migrated the active geospatial core from `raster` to `terra`.
- Added `geospatial_plan()` for explicit validation of raster/vector inputs, CRS, polygon identifiers, and scaling choices.
- Added `extract_ts()` as the unified polygon time-series extraction API with explicit feature IDs and mean/median/min/max/sum aggregation.
- Added product-aware `extract_vpp()`, including VPP parameter interpretation, season/year provenance, official scaling/NoData handling, and date decoding before spatial aggregation.
- Fixed extraction from real HR-VPP raster stacks whose layers share identical internal band descriptions by selecting extracted values positionally while preserving temporal attribution from filenames.
- Original CLMS filenames are preserved for provenance; legacy VPP file renaming is no longer part of the recommended workflow.

## Product quality

- Added `quality_info()`, `classify_quality()`, `summarize_quality()`, and `mask_quality()` for current ST/VPP QFLAG semantics.
- Product quality is explicitly separated from temporal completeness diagnostics.
- Categorical QFLAG rasters are never bilinearly resampled automatically.
- `mask_quality()` supports explicit quality thresholds/codes and requires compatible target/QFLAG raster geometry.

## Time-series quality and analysis

- Added `temporal_plan()` to define expected temporal coverage explicitly.
- Added `summarize_missingness()` to distinguish absent expected dates, explicit `NA` values, and usable observations.
- Added `assess_ts_quality()` for completeness-based whole-series or windowed assessment. It is the primary 2.0 replacement for the legacy `quality.Series()` and `general.Quality()` quality-classification workflows.
- Added `impute_ts()` with explicit linear/Kalman methods, imputation provenance, optional expected-grid completion, long-gap protection, and safeguards against routine re-imputation of already processed ST series.
- Added `model_missingness()` and `plot_missingness()` for GAM-based temporal missingness diagnostics, separating model fitting from plotting.
- Retained selected 1.x functions as deprecated wrappers or migration guards where a safe transition path is available.

## Unified graphical interface

- Added the integrated `TSGenerator2` Shiny application and made it the default interface launched by `runTSapp()`.
- The application follows the workflow Acquire → Prepare/Extract → Quality → Analyze → Export.
- Acquisition, spatial extraction, product quality, temporal analysis, imputation, and export modules call the same public 2.0 R API used in scripts; scientific algorithms are not duplicated in the Shiny server.
- Results & Export can provide available session outputs, acquisition provenance, reproducible R code, session information, and analysis bundles.
- The ST and VPP acquisition mini-apps remain temporarily available for compatibility. Discontinued VI/QFLAG2 and standalone GAM interfaces provide migration guidance instead of silently running obsolete workflows.

## Legacy and dependency changes

- The discontinued VI/QFLAG2 workflow is no longer part of the active 2.0 core. Historical source remains under the build-excluded `legacy/` directory for traceability.
- Removed active dependencies on `reticulate`, `raster`, `plotly`, `VIM`, and `doParallel`.
- Reduced the installed surface to current ST/VPP workflows, the unified Shiny application, and explicitly retained compatibility interfaces.
- Legacy functions tied to discontinued products now act as migration guards where appropriate rather than executing obsolete processing chains.

## Documentation, testing, and release engineering

- Added CRAN-safe vignettes for getting started, ST/VPP workflows, and migration from 1.x to 2.0. Network-dependent examples are not executed during vignette builds.
- Added offline tests for acquisition validation, geospatial extraction, quality handling, temporal completeness, imputation, missingness modelling, installed API availability, Shiny distribution, and package engineering.
- Added regression coverage for malformed dates, explicit non-contiguous QFLAG codes, duplicate HR-VPP raster band descriptions, and release metadata.
- Synchronized migration documentation so `assess_ts_quality()` is the documented primary replacement for `general.Quality()`; `summarize_missingness()` remains the complementary diagnostic summary.
- Prepared package metadata, documentation, build exclusions, and installed assets for the 2.0.0 CRAN release.
