context("hyper-slice")

test_that("slicing works", {
  skip_on_os("solaris")
  l3file <- system.file("extdata/oceandata", 
              "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc", package= "tidync")
  expect_warning(tidync(l3file) %>% hyper_filter() %>% hyper_slice())
  expect_silent(tidync(l3file) %>% hyper_filter() %>% hyper_array())
  tidync(l3file) %>% hyper_filter() %>% hyper_tibble()
  a <- tidync(l3file) %>% hyper_filter(lon = index == 100)
  expect_warning(a %>% hyper_slice())
  expect_silent(a %>% hyper_array())
  
  
  tidync(l3file) %>% hyper_filter(lon = index == 100)
  library(dplyr)
  tidync(l3file) %>% hyper_filter(lat = between(lat, -45, -42), 
                                  lon = lon > 170) %>% hyper_array()
  expect_error(
    ## 
    tidync(l3file) %>% hyper_filter(lat = between(lat, -42, -45), 
                                               lon = lon > 170) %>% 
      hyper_array()
  )
  
  ufile <- system.file("extdata", "unidata", "madis-hydro.nc", 
                       package = "tidync")
  a <- hyper_array(tidync(ufile) %>% activate("D5,D12"))
  expect_named(a, c("precip5minQCD", "precip1hrQCD", "precip3hrQCD", 
                    "precip6hrQCD", "precip12hrQCD", "precip24hrQCD", 
                    "precipAccumQCD"))
  a <- hyper_array(tidync(ufile) %>% activate(precip3hrQCD))
  expect_named(a, c("precip3hrQCD"))
})

