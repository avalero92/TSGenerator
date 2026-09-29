## TSGenerator 2.0.0

- Stable 2.0 release promoted from the fully validated 2.0.0.9012 release candidate.
- Unified Shiny 2.0 interface and Copernicus HR-VPP acquisition, spatial extraction, quality, temporal, missingness, imputation, and export workflows.
- Live WEkEO integration validated for ST and VPP discovery.
- Release-check compliance: portable release metadata test, explicit `GAM.missing()` argument documentation, and NEWS heading normalization.

## TSGenerator 2.0.0.9011 (RC3)

- Makes release-engineering tests independent of the working directory used by R CMD check.
- Synchronizes `mask_quality()` documentation with the validated API.
- Completes argument documentation for active and retained compatibility functions flagged by R CMD check.
- Removes malformed, unused legacy `Barley` data artifacts and non-standard top-level image artifacts.
- Declares Shiny application imports explicitly and removes the unused `dplyr` import.
- Uses `.data` pronouns in `plot_missingness()` to avoid NSE global-binding notes.
- Reformats long Rd usage sections for PDF-manual compatibility.

# TSGenerator 2.0.0.9010 — Release Candidate 2

- Hardened date validation so malformed date strings produce a stable TSGenerator error instead of leaking `charToDate()` errors.
- Fixed `mask_quality()` for `terra::SpatRaster` QFLAG inputs by replacing unsupported `%in%` raster membership with block-wise `terra::app()` membership; categorical QFLAG rasters are still never resampled automatically.
- Clarified the unsupported-raster error to explicitly identify TIFF files and extensions.
- Modernized package-level roxygen documentation to use `_PACKAGE` rather than deprecated `@docType package`.
- Added regression tests for malformed dates and explicit non-contiguous QFLAG keep codes.
- Feature freeze remains in force; RC2 contains only robustness, tests, documentation, and release-engineering changes.

# TSGenerator 2.0.0.9009 — Release Candidate 1

- Feature freeze: no new scientific functionality after the validated unified Shiny 2.0 workflow.
- Release-engineering pass over package metadata, installed API, documentation, tests, vignettes, Shiny distribution, and build exclusions.
- Synchronized documentation for discontinued VI/QFLAG2 migration guards with their active `...` signatures.
- Normalized the LICENSE text to the standard MIT license declared in DESCRIPTION.
- RC1 still requires runtime `R CMD check --as-cran` in an R environment; the build environment used for this static audit does not provide R.

# TSGenerator 2.0.0.9008

## Phase 5.7.6 — Unified Shiny integration and hardening

- Freezes the unified TSGenerator 2.0 Shiny feature set after local validation of phases 5.7.1–5.7.5.
- Connects acquisition session state to Results & Export provenance and the generated reproducible workflow.
- Hardens cross-module state handling so upstream results remain read-only inputs to downstream modules.
- Fixes temporal workflow script downloads to use the same reactive code generator shown in the GUI.
- Adds Shiny distribution/integration regression tests and retains explicit guards for discontinued 1.x interfaces.
- No scientific algorithm was added or changed in this phase.

## TSGenerator 2.0.0.9007 — Phase 5.7.5

- Added unified Results & Export module to the TSGenerator 2.0 Shiny application.
- Consolidates spatial extraction, QFLAG summary, temporal series and temporal quality outputs without duplicating scientific algorithms.
- Added reproducible workflow export, analysis manifest, session information and ZIP analysis bundles.
- Dashboard, acquisition, spatial, quality and temporal modules remain functionally unchanged.


## Phase 5.7.4 — Quality and Time-Series Analysis

- Activated the unified Shiny **Quality** module over `quality_info()`, `summarize_quality()` and `mask_quality()`.
- Activated the **Time-Series Analysis** module over `temporal_plan()`, `summarize_missingness()`, `assess_ts_quality()` and `impute_ts()`.
- Spatial extraction results can flow directly into temporal analysis without reimplementing the geospatial core.
- Added reproducible R-code panels and export controls.
- Preserved the scientific separation between Copernicus QFLAG product quality and temporal completeness.

# TSGenerator 2.0.0.9006

## Phase 5.7.3.1 — HR-VPP extraction regression fix

- Fixed `extract_ts()` for real HR-VPP stacks whose raster layers share identical internal band descriptions.
- Extraction values are now selected positionally from the `terra::extract()` result (feature ID + one value column per raster layer), avoiding duplicate-name collapse caused by name-based set operations.
- Preserved temporal attribution from HR-VPP filenames; repeated GeoTIFF band descriptions are treated as metadata, not temporal identifiers.
- Added a regression test reproducing the real two-layer PPI case discovered during Phase 5.7.3 validation.
- Dashboard, Data Acquisition, and the approved Spatial Processing UI are otherwise unchanged.

# TSGenerator 2.0.0.9004

## Phase 5.7.3 — Unified Shiny: Spatial Processing & Extraction

- Activated the unified **Spatial Processing** module.
- Added GeoTIFF/directory and AOI loading with raster/vector inspection.
- Added CRS, geometry, extent, resolution and attribute diagnostics.
- Added ST/VPP workflow auto-detection with explicit override.
- Integrated `geospatial_plan()`, `extract_ts()` and `extract_vpp()` directly; no scientific algorithm is duplicated in Shiny.
- Added polygon statistic, CRS transformation, exact/touches and NA controls.
- Added extraction preview, CSV export and reproducible R-code generation.
- Dashboard and Data Acquisition modules from phases 5.7.1–5.7.2 are unchanged.

# TSGenerator 2.0.0.9002

## Phase 5.7.2 — Unified Data Acquisition

- Added a functional Data Acquisition module to the unified Shiny 2.0 app.
- ST and VPP preview/search/download actions call the public `download_st()` and `download_vpp()` APIs directly.
- Added live WEkEO diagnostics, ST/VPP product controls, temporal and spatial filters, result tables, output-directory controls, and reproducible R-code generation.
- Corrected live diagnostic labels from ambiguous `schema_` rows to `schema_ST` and `schema_VPP`.
- Development version advanced to 2.0.0.9003.


## Phase 5.7.1 — Unified Shiny Application architecture

- Added the unified `TSGenerator2` Shiny application shell and made it the default target of `runTSapp()`.
- Added a scientific dashboard with environment diagnostics, API availability and the 2.0 workflow.
- Added navigation placeholders for Acquisition, Spatial Processing, Quality, Time-Series Analysis and Results/Export.
- The GUI is explicitly designed as a thin layer over the public 2.0 R API; no scientific algorithms are duplicated in Shiny.
- Retained the existing ST, VPP and GAM mini-apps temporarily for compatibility.

# TSGenerator 2.0.0.9000 — Phase 5.6 / RC1 baseline

- Feature development frozen for the TSGenerator 2.0 release-candidate cycle.
- Added explicit runtime, R CMD check, WEkEO integration, installation, vignette, and Shiny release gates.
- Consolidated Phase 5 development records under the build-excluded `development/` tree.
- Final `2.0.0` version/tag is intentionally withheld until all runtime gates pass.


### Phase 5.2 — Documentation and namespace consolidation

- Audited roxygen documentation across the TSGenerator 2.0 public API.
- Added package-level documentation and API families.
- Added missing help pages for HDA, WEkEO diagnostics, geospatial planning, quality, and temporal planning.
- Simplified NAMESPACE toward explicit namespace-qualified calls.
- Removed the residual `.data` pronoun dependency from temporal plotting.
- Prepared documentation for authoritative regeneration with roxygen2 during Phase 5.3.

## TSGenerator 2.0.0.9000 — Phase 4.1

- Added `temporal_plan()` and the Time-series Quality & Analysis 2.0 validation layer.
- Audited the legacy quality, missingness, imputation and GAM functions.
- Defined separation between HR-VPP product QFLAG and time-series completeness/missingness.
- No legacy analytical algorithm was changed in this phase.

# Phase 3.3

- Added product-aware `extract_vpp()` using terra, official VPP scaling/NoData rules, season/year parsing, and YYDOY-to-Date decoding before spatial aggregation.

# TSGenerator 2.0.0.9000 (development)

### Phase 1 — Clean core

- Created the TSGenerator 2.0 development baseline from the 1.x repository.
- Updated the development version to `2.0.0.9000`.
- Removed `token.txt` from the development working copy and added local credential patterns to `.gitignore` / `.Rbuildignore`.
- No scientific algorithm, public function implementation, raster processing routine, download routine, test, Shiny application, or documentation page has been modified at this baseline step.
- Legacy VI/QFLAG functionality is intentionally still present and will be classified/deprecated in subsequent Phase 1 steps rather than deleted without a transition plan.

## Phase 1.2 — structure sanitation and legacy API

- Classified the discontinued VI/QFLAG workflow as legacy without deleting its public functions.
- Consolidated duplicate date parsing code.
- Corrected the `count.NA()` namespace registration.
- Replaced the `tidyverse` meta-dependency with direct `tidyr` use.
- Aligned package metadata and license naming with the 2.0 development direction.

## Phase 1.3 — dependency and Shiny sanitation

- Reduced the packaged Shiny surface to the active ST, VPP, and GAM interfaces.
- Archived legacy VI/QFLAG GUIs outside the package build.
- Removed duplicate Shiny implementation files from active app directories.
- Removed redundant manual sourcing of `global.R`.
- Updated `runTSapp()` for the active 2.0-development interface set.
- Reconciled DESCRIPTION with dependencies referenced by the packaged active applications.

## Phase 1.4 — Core consolidation

- Added `count_missing()` as the canonical missing-data summary API; retained `count.NA()` as a compatibility wrapper.
- Consolidated TIFF date parsing into one internal utility.
- Removed `mrplot()` and `VIM` from the active package; historical source is retained under `legacy/`.
- Added initial core utility tests and finalized the clean-core boundary before Phase 2 acquisition work.

## Phase 2.1 — Native WEkEO backend

* Added a native R acquisition backend based on `hdar`/HDA V2.
* Added client, authentication check, dataset discovery, live query-template, search and download wrappers.
* New backend does not use Python or `reticulate`.
* Existing ST/VPP download functions remain unchanged pending Phases 2.2–2.3.

## Phase 2.2 — Native Seasonal Trajectories acquisition

- Added `download_st()` for native R acquisition of HR-VPP ST through WEkEO HDA V2.
- Added PPI/QFLAG selection, tile and bbox filters, date validation, preview mode, and structured return objects.
- Added automatic splitting of long requests into maximum 31-day windows.
- Added offline tests for ST validation and HDA query construction.
- Kept legacy `Download.STPPI()` unchanged pending integration testing and GUI migration.

## Phase 2.3 - Native VPP acquisition

- Added `download_vpp()` for native R acquisition of Copernicus HR-VPP VPP products through WEkEO HDA V2 and `hdar`.
- Added support for the current VPP product types: MINV, MAXD, LENGTH, SOSD, QFLAG, EOSV, TPROD, MAXV, AMPL, SOSV, LSLOPE, EOSD, RSLOPE, and SPROD.
- Added explicit `s1`/`s2` season-group handling through `productGroupId`, plus an unfiltered mode.
- Added tile, bounding-box, platform, product-version, preview, overwrite, and structured-return support.
- Added offline unit tests for VPP validation and query construction.

## Phase 2.4 - acquisition robustness

- Added `check_wekeo()` for offline/online diagnostics of the native acquisition stack.
- Added bounded exponential retry support to HDA searches and downloads.
- Added request-level transfer-size estimates to `download_st()` and `download_vpp()` summaries.
- Preserved skip-existing behavior to reduce unnecessary transfers and WEkEO quota use.
- Added offline tests for retry logic, diagnostics, and size accounting.

## TSGenerator 2.0.0.9000 — Phase 2.5

* Completed migration of active ST/VPP acquisition to native R (`hdar` + HDA V2).
* Removed `reticulate` from the package core and from `DESCRIPTION`/`NAMESPACE`.
* Replaced `Download.STPPI()` and `Download.HRVPP()` with deprecated compatibility wrappers over `download_st()` and `download_vpp()`.
* Replaced the active ST and VPP Shiny download interfaces with native-R interfaces; Python-path configuration is no longer exposed.
* `Download.VI()` now fails explicitly with migration guidance and contains no Python dependency; its historical implementation is retained under `legacy/R`.
* Promoted `hdar` and `jsonlite` to `Imports` because they are now required by the acquisition core.
* Removed no-longer-used GUI dependencies `magick`, `fs`, and `shinyFiles` from the active package.

## Phase 3.1 — Geospatial core design

* Added the terra-first geospatial engine infrastructure and `geospatial_plan()`.
* Added strict raster/vector input, CRS, polygon ID, and scaling validation.
* Established explicit design contracts for `extract_ts()`, `extract_vpp()`, and ST quality processing.
* Kept the 1.x raster-based extraction API unchanged pending its staged replacement in phases 3.2–3.5.

## Phase 3.2 - unified time-series extraction

- Added `extract_ts()` as the terra-first polygon time-series extractor.
- Unified mean/median/min/max/sum extraction behind one API.
- Added explicit polygon IDs, CRS alignment, date resolution, scaling/offset, and tidy output.
- Added `tsg_time_series` output class and offline synthetic-raster tests.
- Legacy `get.Series.mean()` and `get.Series.median()` remain unchanged for compatibility until Phase 3.5.

## Phase 3.4 - HR-VPP quality infrastructure

* Added product-aware ST and VPP quality-code definitions.
* Added `quality_info()`, `classify_quality()`, `summarize_quality()` and `mask_quality()`.
* ST-QFLAG codes 0-5 are preserved with their documented confidence semantics; the default mask retains 3-5.
* VPP-QFLAG codes 0-10 are preserved; the default mask retains medium/high confidence codes 7-10.
* Quality rasters are never bilinearly resampled. `mask_quality()` requires identical target/QFLAG geometry.
* Added offline tests for quality classification, summaries and masking behaviour.

## TSGenerator 2.0.0.9000 — Phase 3.5

- Completed the active geospatial migration from `raster` to `terra`.
- Removed `raster`, `parallel`, and `doParallel` from package dependencies.
- Deprecated `get.Series.mean()` and `get.Series.median()` as wrappers around `extract_ts()`.
- Retired legacy VI/QFLAG2 geospatial execution from the installed core and archived historical implementations under `legacy/R/`.
- Retired `get.Series.VPP()` in favor of product-aware `extract_vpp()`.
- Retired VPP file renaming; VPP provenance is now preserved and parsed directly.

## Phase 4.2 — Time-series quality and missingness

* Added `summarize_missingness()` to distinguish absent expected dates, explicit `NA` values, and usable observations.
* Added `assess_ts_quality()` for completeness-based whole-series or windowed quality assessment.
* Removed the 91-day window as a hard-coded analytical assumption; it is now an optional `window_days` setting.
* Kept temporal completeness explicitly separate from Copernicus ST/VPP QFLAG product quality.

## Phase 4.3 — Imputation 2.0
- Added `impute_ts()` with explicit linear/Kalman methods and imputation provenance.
- Added safeguards against routine re-imputation of Copernicus ST products.
- Added optional expected-grid completion, long-gap protection, and edge controls.
- Deprecated `TsImpute()` in favour of `impute_ts()`.
- Moved `imputeTS` from Imports to Suggests.

## Phase 4.4

- Added `model_missingness()` and `plot_missingness()` for GAM-based temporal missingness diagnostics.
- Year is now categorical by default and DOY uses a cyclic smooth.
- Deprecated `GAM.missing()` and `GAM.plot()` in favour of the 2.0 API.

## Phase 4.5 - Temporal core consolidation

- Consolidated the TSGenerator 2.0 temporal-analysis API around `temporal_plan()`, `summarize_missingness()`, `assess_ts_quality()`, `impute_ts()`, `model_missingness()`, and `plot_missingness()`.
- Retired the active implementations of `quality.Series()`, `general.Quality()`, and the phenology-specific legacy `count_missing()` workflow; their historical code is archived under `legacy/R/` and exported names now provide migration guidance.
- Retained `TsImpute()`, `GAM.missing()`, and `GAM.plot()` only as deprecated compatibility wrappers around the 2.0 API.
- Migrated the bundled GAM Shiny application to `model_missingness()` and the standardized 2.0 result columns.
- Removed `plotly` from package dependencies and namespace; interactive plotting is no longer a hidden side effect of analytical functions.
## Phase 5.1 — package engineering audit

- Audited architecture, public API, dependencies, tests, bundled Shiny apps, and package-root hygiene.
- Removed unused runtime imports `sf`, `rlang`, `lubridate`, and `tidyr`.
- Moved development history/assets under `development/` and excluded them from package builds.
- Identified documentation regeneration, full R checks, live WEkEO integration, and legacy-export policy as release blockers.


### Phase 5.3 — verification hardening

- Removed stale tests for the discontinued Python/reticulate VI downloader.
- Removed the obsolete Plotly/count_missing test inherited from the 1.x API.
- Added package-engineering tests for the primary 2.0 API and retired dependencies.
- Performed static source, namespace, documentation, dependency and test-suite consistency checks.
- Full runtime validation with `devtools::document()`, `testthat`, installation and `R CMD check` remains mandatory in an R environment; it was not falsely marked as executed here.

## Phase 5.4

* Added `check_wekeo_integration()` for opt-in live ST/VPP end-to-end smoke tests.
* ST and VPP acquisition now resolve temporal parameter names from the live WEkEO queryable schema instead of assuming a fixed `start`/`end` convention.
* Added opt-in `testthat` live integration coverage controlled by `TSGENERATOR_LIVE_WEKEO=true`.

## Phase 5.5 — user documentation and migration

- Rebuilt the README around the TSGenerator 2.0 architecture and current ST/VPP workflows.
- Removed legacy 1.x examples from the primary user-facing workflow.
- Added vignettes for getting started, ST/VPP workflows, and migration from 1.x to 2.0.
- Documented the distinction between Copernicus product quality and TSGenerator temporal completeness.
- Documented the explicit policy against routine re-imputation of processed ST products.
- Added `knitr`/`rmarkdown` as vignette-only suggested dependencies and declared `VignetteBuilder: knitr`.

## TSGenerator 2.0.0.9001 — Phase 5.6.1

### RC installation artifact fix

- Reissued the RC validation baseline as version 2.0.0.9001 so a successful clean installation can be distinguished unambiguously from the defective 2.0.0.9000 installation artifact.
- Added an installed-namespace smoke test covering the primary 2.0 API.
- Added a release installation verification script.
- Distribution now includes a conventional R source tarball (`TSGenerator_2.0.0.9001.tar.gz`) for clean installation. The project ZIP is retained for source inspection/development only and must not be installed as a Windows binary.
