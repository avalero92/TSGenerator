test_that("Download.VI is a deterministic migration guard", {
  expect_error(
    TSGenerator::Download.VI(),
    "no longer operational.*discontinued"
  )
  expect_error(
    TSGenerator::Download.VI(user = "u", password = "p", dataset_id = "legacy"),
    "download_st\\(\\).*download_vpp\\(\\)"
  )
})
