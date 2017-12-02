test_that("tidync works", {
  skip_if_not(we_are_raady())
 
  afile <- "/rdsi/PUBLIC/raad/data/ftp.sltac.cls.fr/Core/SEALEVEL_GLO_PHY_L4_REP_OBSERVATIONS_008_047/dataset-duacs-rep-global-merged-allsat-phy-l4-v3/1993/dt_global_allsat_phy_l4_19930101_20170110.nc"
  afilter <- tidync(afile)  %>% expect_s3_class("tidync") %>% 
    hyper_filter() %>% expect_s3_class("tidync")  
  afilter2 <- tidync(afile) %>%   hyper_filter(latitude = latitude > 60) %>% expect_s3_class("tidync") 
  expect_that(afilter %>% hyper_slice() %>% `[[`(1) %>% dim() , equals(c(1440, 720)))
  expect_that(afilter2 %>% hyper_slice() %>% `[[`(1) %>% dim() , equals(c(1440, 120)))
  afilter2 %>% hyper_tibble() %>% expect_s3_class("tbl_df") 
})

context("expected errors")
test_that("recorded failures", {
  skip_if_not(we_are_raady())
  #   Error in ncvar_type_to_string(rv$precint) : 
  #  Error, unrecognized type code of variable supplied: -1 
  l3bin <- "/rdsi/PUBLIC/raad/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/L3BIN/2005/206/S2005206.L3b_DAY_RRS.nc"
  #expect_error(tidync(l3bin), "no variables or dimension recognizable")
  ## changed December 2017
  expect_error(tidync(l3bin), "HDF error")
  
  # Error in hyper_filter(., x = x < 30) : object 'x' not found
  (f <- "/rdsi/PRIVATE/raad/data_local/amps/2013021900_WRF_d2_PCP_f009.nc")
  PCP <- tidync(f) %>% activate(PCP)

  ## FIXME: https://github.com/hypertidy/tidync/issues/30
  #expect_weirdass_non_error(print(PCP %>% hyper_filter()))
  #expect_error(PCP %>% hyper_filter(x = x < 30), "object 'x' not found")
 # expect_error(PCP %>% hyper_filter(x = x < 30), "(list) object cannot be coerced to type 'double'")
  ##expect_error(PCP %>% hyper_filter(x = x < 30))
})
