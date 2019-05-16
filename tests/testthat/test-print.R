context("test-print.R")

test_that("print works", {
  ufile <- system.file("extdata", "unidata", "madis-hydro.nc", 
                       package = "tidync")
  
  tnc <- expect_s3_class(tidync(ufile), "tidync")
  expect_output(print(tnc))
  
  ab <- expect_output(print(hyper_array(tidync(ufile), select_var = c('precip6hrQCD', 'precip12hrQCD'))))
  expect_s3_class(ab, "tidync_data")
  expect_length(ab, 2)
  expect_named(ab, c('precip6hrQCD', 'precip12hrQCD'))
  expect_named(attr(ab, "transforms"), c("QCcheckNum", "recNum"))
  
  tst <- hyper_tibble(tnc, QCcheckNum = dplyr::between(index, 2, 8), 
                           recNum = recNum > 1100, na.rm = FALSE)
})
