## TSGenerator 2.0.1 maintenance update

This submission updates the existing CRAN package TSGenerator from version 2.0.0 to 2.0.1.

### Changes

- Corrected VPP scale/offset handling to avoid double application of product scale factors when terra has already applied GDAL metadata.
- Added regression tests covering scaled and unscaled VPP raster inputs.
- Improved Shiny raster input workflows and direct-disk inspection of large raster collections.
- Made the browser upload limit configurable (default 512 MiB).
- Updated release metadata, application version display, and citation documentation.

No intentional changes to the public API.

### Checks

- Windows x86_64, R 4.5.1: R CMD check --as-cran completed with 0 ERRORs, 0 WARNINGs, and 0 NOTEs.
- Source package built successfully and installed from source.
- Local pre-release testthat results: 216 passed, 0 failed, 0 warnings, 1 intentional skip.
- Local pre-release devtools::check(args = '--no-manual'): 0 errors, 0 warnings, 0 notes.
- GitHub Actions R CMD check workflow succeeded on master (runs #78, #79, and #80).
- Installed package version and citation verified for version 2.0.1.
- No direct reverse dependencies identified in the CRAN package index for Depends, Imports, or LinkingTo.

### Release and citation

- GitHub release: https://github.com/avalero92/TSGenerator/releases/tag/v2.0.1
- Version-specific Zenodo DOI: https://doi.org/10.5281/zenodo.23620160
- The GitHub release archive predates documentation-only updates to inst/CITATION and README.md on master. The scientific implementation was unchanged.

### Submission artifact

- File: TSGenerator_2.0.1.tar.gz
- Size: 296245 bytes
- SHA-256: f28297e1551b3608ecdd295ca9fb1ffab394d190dbfafa00a6288ae09f249b37

The source archive was verified to contain version 2.0.1.
The CRAN package index listed TSGenerator 2.0.0 before this submission.
