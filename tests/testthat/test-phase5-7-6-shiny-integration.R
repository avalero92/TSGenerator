test_that("unified Shiny application is the default interface", {
  f <- system.file("app", "TSGenerator2", "app.R", package = "TSGenerator")
  expect_true(nzchar(f))
  expect_true(file.exists(f))
  expect_true(dir.exists(system.file("app", "TSGenerator2", package = "TSGenerator")))
})

test_that("all unified Shiny modules are distributed", {
  app <- system.file("app", "TSGenerator2", package = "TSGenerator")
  modules <- c("mod_dashboard.R", "mod_acquisition.R", "mod_spatial.R",
               "mod_quality.R", "mod_temporal.R", "mod_export.R")
  expect_true(all(file.exists(file.path(app, "R", modules))))
})

test_that("legacy discontinued apps are guarded by runTSapp", {
  expect_error(runTSapp("DownloadVI"), "discontinued 1.x")
  expect_error(runTSapp("not-an-app"), "Invalid app")
})
