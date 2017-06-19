context("files")

fname <- paste(sample(unlist(strsplit("somecrazyfile", ""))), collapse = "")
test_that("file not found is friendly", {
  expect_error(tidync(fname), "No such file or directory")
})

test_that("files and sets of files are handled", {
  fname <- raadtools::sstfiles()$fullname[1]
  tidync(fname)
})
