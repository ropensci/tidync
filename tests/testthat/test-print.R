context("test-print.R")

test_that("print works", {
  ufile <- system.file("extdata", "unidata", "madis-hydro.nc", 
                       package = "tidync")
  
  expect_output(print(tidync(ufile)))
})
