context("thredds")

test_that("thredds works", {
  skip_on_travis()
  skip_on_cran()
  library(tidync)
  u <- "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day"
  unc <- tidync(u)
  expect_named(unc, c("source", "axis", "grid", 
                      "dimension", "variable", "transforms"))
  
  
})


