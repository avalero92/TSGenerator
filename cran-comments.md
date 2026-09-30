## Test environments

* GitHub Actions:
  * Ubuntu, R-devel
  * Ubuntu, R-release
  * Ubuntu, R-oldrel-1
  * Windows, R-release
  * macOS, R-release
* win-builder: Windows Server 2022, R-devel (2026-09-29 r90598 ucrt)
* R-hub noSuggests: Fedora Linux 42, R-devel

## R CMD check results

0 errors | 0 warnings | 0 notes

The CRAN source package was built and checked with `R CMD check --as-cran`
on Ubuntu using R-devel. The same package source was also checked successfully
across the GitHub Actions test matrix listed above.

## win-builder

0 errors | 0 warnings | 1 note

The NOTE reports that this is a new submission and identifies "Phenology",
"TSGenerator", "VPP", and "geospatial" as possibly misspelled words in
DESCRIPTION. These are correctly spelled scientific/technical terms, the
package name, and the standard abbreviation VPP (Vegetation Phenology and
Productivity).

## Additional checks

R-hub noSuggests:

0 errors | 0 warnings | 0 notes

The package installs, loads, runs its examples and tests, and rebuilds its
vignettes successfully when the optional `imputeTS` package is unavailable.

## Submission

This is a new submission.