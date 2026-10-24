## TSGenerator 2.0.1 maintenance update

This submission updates the existing CRAN package TSGenerator from version 2.0.0 to 2.0.1.

### Changes

- Corrected VPP scale/offset handling to avoid double application of product scale factors when `terra` has already applied GDAL metadata.
- Added regression tests covering scaled and unscaled VPP raster inputs.
- Improved Shiny raster input workflows and direct-disk inspection of large raster collections.
- Made the browser upload limit configurable (default 512 MiB).
- Updated release metadata, application version display, and citation documentation.

No intentional changes to the public API.

### Release and citation

- GitHub release: https://github.com/avalero92/TSGenerator/releases/tag/v2.0.1
- Version-specific Zenodo DOI: https://doi.org/10.5281/zenodo.23620160
- The archived GitHub release predates two subsequent documentation-only commits to `master` that added the DOI to `inst/CITATION` and `README.md`. The scientific implementation was not changed by those commits.

### Checks already completed

- Local pre-release `testthat::test_local()`: 216 passed, 0 failed, 0 warnings, 1 intentional skip.
- Local pre-release `devtools::check(args = "--no-manual")`: 0 errors, 0 warnings, 0 notes.
- GitHub Actions R CMD check workflow succeeded on `master` after the merge and after both DOI documentation commits (runs #78, #79, #80). This statement refers to workflow-level results; platform-specific logs should be retained for the final submission record.

### Final checks before CRAN submission

- [ ] Synchronize the local checkout with the final `master` commit.
- [ ] Verify `citation("TSGenerator")` from an installed build and confirm the 2.0.1 DOI.
- [ ] Run `R CMD build` and `R CMD check --as-cran` on the final source package; record actual E/W/N results and platform.
- [ ] Review spelling and CRAN incoming-check output; document any findings.
- [ ] Consider reverse dependencies and document the outcome.
- [ ] Confirm the submitted tarball matches the final checked source.

These pending checks are not claimed as completed.
