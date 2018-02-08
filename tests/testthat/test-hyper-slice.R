context("hyper-slice")

test_that("slicing works", {
  l3file <- system.file("extdata/oceandata", 
              "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc", package= "tidync")
  tidync(l3file) %>% hyper_filter() %>% hyper_slice()
  tidync(l3file) %>% hyper_filter() %>% hyper_tibble()
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

