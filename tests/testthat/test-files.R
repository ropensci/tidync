context("files")

fname <- paste(sample(unlist(strsplit("somecrazyfile", ""))), collapse = "")
test_that("file not found is friendly", {
  expect_error(tidync(fname), "failed to open")
})
