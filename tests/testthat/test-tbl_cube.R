context("tbl_cube")

test_that("tbl_cube works", {
  f <- system.file("extdata", "ifremer", "20171002.nc", package = "tidync")
  expect_error(hyper_tbl_cube(f), "direct file access not yet supported")
  hyper_tbl_cube(tidync(f)) %>% expect_s3_class("tbl_cube")
}
          
          )

test_that("tbl_cube works for raad", {
  #f <- "eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/1981/AVHRR/avhrr-only-v2.19810901.nc"
  skip_if_not(we_are_raady())
  f <- raadtools::sstfiles()$fullname[10000]
  tidync(f) %>% hyper_tbl_cube(lat = lat > -30)
  
  # raadfiles:::get_raw_raad_filenames() %>% dplyr::filter(grepl("wkmean", file))
  f <- "/rdsi/PUBLIC/raad/data/ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.wkmean.1990-present.nc"
  
  library(dplyr)
  htc <- tidync(f) %>% hyper_filter(time = index < 10) %>% 
    hyper_tbl_cube() 
  
  expect_true(all(dim(htc$mets$sst) == lengths(htc$dims)[1:2]))
})

