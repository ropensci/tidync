context("test-nc_char")

f <- system.file("extdata/argo/MR5905167_372.nc", package = "tidync", mustWork = TRUE)
test_that("nc_char is handled gracefully", {
  tnc <- tidync(f)
  print(tnc)
})
