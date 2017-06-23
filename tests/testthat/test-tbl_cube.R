context("tbl_cube")

test_that("multiplication works", {
  f <- "eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/1981/AVHRR/avhrr-only-v2.19810901.nc"
  tidync(f) %>% hyper_tbl_cube(lat = lat > -30)
  
  
})
