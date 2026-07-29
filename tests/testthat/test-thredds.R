context("thredds")

test_that("thredds works", {
  skip_on_cran()
  skip_if_offline()
  library(tidync)
  u <- "https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day"
  unc <- tidync(u)
  expect_named(unc, c("source", "axis", "grid", 
                      "dimension", "variable", "extended",
                      "attribute", "transforms"))
})


