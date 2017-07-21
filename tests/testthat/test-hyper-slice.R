context("hyper-slice")

test_that("slicing works", {
  l3file <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump")
  tidync(l3file) %>% hyper_index() %>% hyper_slice()
  
  a <- tidync(l3file) %>% hyper_filter(lon = index == 100) %>% hyper_slice()
  tidync(l3file) %>% hyper_filter(lon = index == 100)
  library(dplyr)
  tidync(l3file) %>% hyper_filter(lat = between(lat, -45, -42), 
                                  lon = lon > 170) %>% hyper_slice()
  expect_error(
    ## 
    tidync(l3file) %>% hyper_filter(lat = between(lat, -42, -45), 
                                               lon = lon > 170) %>% hyper_slice()
  )
})

#test_that("warn/confirm on crazy big", {
#  stopifnot(we_are_raady())
#  f <- raadtools::cpolarfiles()$fullname[1]
#  #a <- hyper_filter(f, xi_rho = index < 400) %>% hyper_slice(select_var = "salt")
#
#})