context("thredds")

test_that("thredds works", {
  skip_on_travis()
  library(tidync)
  u <- "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day"
  (unc <- tidync(u))
  
  unc %>% activate("D2")
  
  unc %>% hyper_filter(longitude = index < 100, latitude = latitude > 60, time = index == 1)
  
})
