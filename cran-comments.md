## TSGenerator 2.0.1 maintenance update

This is a maintenance update to TSGenerator 2.0.0.

- Corrected VPP scale/offset handling to avoid double application of product scale factors when `terra` already applies GDAL metadata.
- Added regression coverage for VPP scaling.
- Improved Shiny raster input workflows and made browser upload size configurable (default 512 MiB). Large raster collections can be read from disk without HTTP upload.
- Updated version metadata and application version display.

Final CRAN submission checks (R CMD check, spelling, incoming checks, reverse dependency considerations) must be recorded here before submission. The new version-specific Zenodo DOI is pending release and is not claimed here.
