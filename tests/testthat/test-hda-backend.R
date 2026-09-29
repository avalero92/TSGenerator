test_that("HDA backend validates credential pairs", {
  expect_error(hda_client(username = "user"), "both")
  expect_error(hda_client(password = "pass"), "both")
})

test_that("dataset identifiers are validated before network access", {
  expect_error(hda_query_template(""), "dataset_id")
  expect_error(hda_query_template(NULL), "dataset_id")
})

test_that("download destination is validated before method dispatch", {
  expect_error(hda_download(NULL, tempdir()), "results")
  expect_error(hda_download(list(), ""), "output_dir")
})
