context("test-nc_char")

f <- system.file("extdata/argo/MR5905167_372.nc", 
                 package = "tidync", mustWork = TRUE)
test_that("nc_char is handled gracefully", {
  tnc <- tidync(f)
  expect_s3_class(tnc$variable, "tbl_df")
  expect_true(all(dim(tnc$variable) == c(77L, 7L)))
  expect_output(print(tnc))
})
