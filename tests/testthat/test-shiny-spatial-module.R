test_that("unified Shiny spatial module is packaged", {
  app <- system.file("app", "TSGenerator2", package = "TSGenerator")
  skip_if(!nzchar(app), "Installed app directory unavailable in source-only test context")
  expect_true(file.exists(file.path(app, "R", "mod_spatial.R")))
  txt <- paste(readLines(file.path(app, "R", "mod_spatial.R"), warn = FALSE), collapse = "\n")
  expect_match(txt, "TSGenerator::geospatial_plan", fixed = TRUE)
  expect_match(txt, "TSGenerator::extract_ts", fixed = TRUE)
  expect_match(txt, "TSGenerator::extract_vpp", fixed = TRUE)
})
