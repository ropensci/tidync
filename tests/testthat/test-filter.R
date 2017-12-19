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
hf <-  tidync(f2) %>% hyper_filter(y = index < 10,x = index < 5)
hf %>% hyper_slice()
  
  })

test_that("indexing works", {
  skip_on_cran()
  l3file <- system.file("extdata/oceandata", 
              "S20092742009304.L3m_MO_CHL_chlor_a_9km.nc", package= "tidync")
  ind0 <- tidync(l3file) %>% hyper_filter()
  expect_that(ind0$dimension$count[ind0$dimension$active], equals(c(2160L, 4320L)))
  
  ind1 <- tidync(l3file) %>% hyper_filter(lon = index == 100) 
  
  expect_that(ind1$dimension$count[ind0$dimension$active], equals(c(2160L, 1L)))
  expect_warning(ind2 <- tidync(l3file) %>% hyper_filter(lon = index %% 100 == 0))
  expect_that(ind2$dimension$count[ind2$dimension$active], equals(c(2160, 4201)))  
  
})

