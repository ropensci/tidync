context("filter")

library(dplyr)
test_that("standard mapped", {
  skip_if_not(we_are_raady())
  oisst_dayfile <- raadfiles::oisst_daily_files()$fullname[1]
  tidync(oisst_dayfile) %>% hyper_filter()
  tidync(oisst_dayfile) %>% hyper_filter(lon = lon > 100, lat = index < 10)
  
  ## dimensions without variables
  f1 <- system.file("extdata/unidata/madis-hydro.nc", package = "tidync")
  tidync(f1) %>% activate("D5,D12") %>% hyper_filter(QCcheckNum  = index < 2)
  tidync(f1) %>% activate("D5,D12") %>% hyper_filter()
  f2 <- raadfiles:::get_raw_raad_filenames() %>% 
    dplyr::filter(grepl("2013021900_WRF_d2_PCP_f009.nc", file)) %>% 
    dplyr::mutate(fullname = file.path(root, file)) %>% dplyr::pull(fullname)
  tidync(f2) %>% hyper_filter()
  # https://github.com/hypertidy/tidync/issues/13
 tidync(f2) %>% hyper_filter(y = index < 10,x = index < 5)
  
  })


## FIXME: we need a lot more examples!  
test_that("non-local sources work", {
  skip_on_travis()
  u <- "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day"
  tidync(u) %>% hyper_filter()
  ht <- tidync(u) %>% hyper_filter(longitude = longitude > 150 & longitude < 180, 
                             latitude = latitude > -10 & latitude < 10, 
                             time = index < 2) %>% 
    hyper_tibble()
  
  
})
