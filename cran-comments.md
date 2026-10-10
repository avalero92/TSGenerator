## Resubmission

This is a resubmission of TSGenerator 2.0.0 following the CRAN review.
Thank you for the feedback. The following changes address the review comments:

1. **Package title and description:** Removed the redundant "Tools for"
   wording from the package Title in DESCRIPTION. The title now reads
   "'Copernicus' HR-VPP Vegetation Time-Series Processing".

2. **Return-value documentation:** Reviewed and improved the `\\value{}`
   sections of the Rd documentation, particularly for
   `assess_ts_quality()`, `count.NA()`, `get.Series.VPP()`, and
   `summarize_missingness()`. The documentation now distinguishes
   structured return values from deprecated legacy functions that terminate
   with an informative error. Corrected the legacy `count_missing()`
   documentation accordingly.

3. **Examples:** Reviewed the use of `\\dontrun{}` in examples and
   replaced it with runnable or `\\donttest{}` examples where
   appropriate. Examples that require external authenticated WEkEO access
   or launch interactive Shiny applications remain protected to avoid
   network-dependent or interactive execution during CRAN checks.

4. **Writing files:** Download functions now require an explicitly supplied
   destination when `download = TRUE`; no download destination is chosen
   implicitly from the package directory, working directory, or home
   directory. The WEkEO integration helper follows the same rule.
   Examples and test workflows use temporary directories where applicable,
   and the Shiny interface does not prefill output paths.

The README and the new `inst/CITATION` file also cite the archived
TSGenerator 2.0.0 software release (DOI: 10.5281/zenodo.23265767).

## Test environments

* GitHub Actions:
  * Ubuntu, R-devel (`--as-cran`)
  * Ubuntu, R-devel
  * Ubuntu, R-release
  * Ubuntu, R-oldrel-1
  * Windows, R-release
  * macOS, R-release
* R-hub noSuggests: Fedora Linux 42, R-devel
* Earlier win-builder check: Windows Server 2022, R-devel
  (2026-09-29 r90598 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes

The current resubmission branch passed the GitHub Actions R CMD check
matrix, including `R CMD check --as-cran` on Ubuntu R-devel, on
2026-10-10 (workflow run 68):
https://github.com/avalero92/TSGenerator/actions/runs/38061179790

The R-hub noSuggests job in the same workflow also passed.

## Earlier win-builder result

0 errors | 0 warnings | 1 note

The earlier NOTE identified this as a new submission and flagged
"Phenology", "TSGenerator", "VPP", and "geospatial" as possible spelling
issues in DESCRIPTION. These are scientific/technical terms, the package
name, and the abbreviation for Vegetation Phenology and Productivity.
This earlier win-builder result predates the final resubmission changes
and is not presented as a fresh check of the current commit.

## External services

Online searches and downloads require authenticated access to the
external WEkEO service. Package checks and tests do not require
WEkEO credentials or live network access.
