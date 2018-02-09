context("tbl_cube")

test_that("tbl_cube works", {
  f <- system.file("extdata", "ifremer", "20171002.nc", package = "tidync")
  expect_s3_class(hyper_tbl_cube(f), "tbl_cube")
  hyper_tbl_cube(tidync(f)) %>% expect_s3_class("tbl_cube")
})


