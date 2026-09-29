test_that("unified Shiny 2.0 shell is installed", {
  app_dir <- system.file("app", "TSGenerator2", package = "TSGenerator")
  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "R", "mod_dashboard.R")))
  expect_true(file.exists(file.path(app_dir, "www", "tsgenerator.css")))
})

test_that("runTSapp defaults to unified interface", {
  expect_identical(formals(runTSapp)$app, "TSGenerator2")
})
