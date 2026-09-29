test_that("unified Shiny app includes the acquisition module", {
  app_dir <- system.file("app", "TSGenerator2", package = "TSGenerator")
  expect_true(nzchar(app_dir))
  expect_true(file.exists(file.path(app_dir, "R", "mod_acquisition.R")))
  app <- paste(readLines(file.path(app_dir, "app.R"), warn = FALSE), collapse = "\n")
  mod <- paste(readLines(file.path(app_dir, "R", "mod_acquisition.R"), warn = FALSE), collapse = "\n")
  expect_match(app, "mod_acquisition_ui", fixed = TRUE)
  expect_match(app, "mod_acquisition_server", fixed = TRUE)
  expect_match(mod, "TSGenerator::download_st", fixed = TRUE)
  expect_match(mod, "TSGenerator::download_vpp", fixed = TRUE)
  expect_match(mod, "TSGenerator::check_wekeo", fixed = TRUE)
  expect_match(mod, "download = FALSE", fixed = TRUE)
})

