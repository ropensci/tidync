test_that("tidync works", {
  skip_if_not(we_are_raady())
  afile <- "/rdsi/PRIVATE/raad/data/ftp.aviso.altimetry.fr/global/delayed-time/grids/madt/all-sat-merged/h/2009/dt_global_allsat_madt_h_20090104_20140106.nc"
  afilter <- tidync(afile)  %>% expect_s3_class("tidync") %>% 
    hyper_filter() %>% expect_s3_class("hyperfilter")  
  afilter2 <- tidync(afile) %>%   hyper_filter(lat = lat > 60) %>% expect_s3_class("hyperfilter") 
  expect_that(afilter %>% hyper_slice() %>% dim() , equals(c(2, 720)))
  expect_that(afilter2 %>% hyper_slice() %>% dim() , equals(c(2, 120)))
  afilter2 %>% hyper_tibble() %>% expect_s3_class("tbl_df") 
})

context("expected errors")
test_that("recorded failures", {
  skip_if_not(we_are_raady())
  #   Error in ncvar_type_to_string(rv$precint) : 
  #  Error, unrecognized type code of variable supplied: -1 
  l3bin <- "/rdsi/PRIVATE/raad/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/2005/206/S2005206.L3b_DAY_RRS.nc"
  expect_warning(tidync(l3bin), "no variables recognizable")
  
  # Error in hyper_filter(., x = x < 30) : object 'x' not found
  (f <- "/rdsi/PRIVATE/raad/data_local/amps/2013021900_WRF_d2_PCP_f009.nc")
  PCP <- tidync(f) %>% activate(PCP)
  PCP %>% hyper_filter()
  expect_error(PCP %>% hyper_filter(x = x < 30), "object 'x' not found")
  
})
