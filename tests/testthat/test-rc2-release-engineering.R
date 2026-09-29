test_that("release metadata and legacy guard documentation are synchronized", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "TSGenerator"))
  expect_match(unname(desc[1, "Version"]), "^2\\.0\\.0(?:\\.[0-9]+)?$")

  guards <- c("Download.VI", "get.Stack", "get.Clean.IV", "QFLAG2.Mask", "renames.image.IV")
  ns <- asNamespace("TSGenerator")
  expect_true(all(vapply(guards, exists, logical(1), envir = ns, inherits = FALSE)))
  expect_true(all(guards %in% getNamespaceExports("TSGenerator")))
})
