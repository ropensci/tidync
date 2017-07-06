context("filter")

library(dplyr)
test_that("standard mapped", {
  skip_if_not(we_are_raady())
  oisst_dayfile <- raadtools::sstfiles()$fullname[1]
  tidync(oisst_dayfile) %>% hyper_filter()
  tidync(oisst_dayfile) %>% hyper_filter(lon = lon > 100, lat = index < 10)
  
  ## dimensions without variables
  f1 <- system.file("extdata/unidata/madis-hydro.nc", package = "tidync")
  tidync(f1) %>% hyper_filter()
  f2 <- raadfiles:::get_raw_raad_filenames() %>% 
    dplyr::filter(grepl("2013021900_WRF_d2_PCP_f009.nc", file)) %>% 
    dplyr::mutate(fullname = file.path(root, file)) %>% dplyr::pull(fullname)
  tidync(f2) %>% hyper_filter()
  tidync(f2) %>% hyper_filter(y = index < 10,x = index < 5)
  
  })


## FIXME: we need a lot more examples!  
test_that("non-local sources work", {
  u <- "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day"
  tidync(u) %>% hyper_filter()
  tidync(u) %>% hyper_filter()
  
})