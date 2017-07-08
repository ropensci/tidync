context("index")

test_that("indexing works", {
  l3file <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump")
  ind0 <- tidync(l3file) %>% hyper_index()
  expect_that(ind0$count, equals(c(4320L, 2160L)))
  
  ind1 <- tidync(l3file) %>% hyper_filter(lon = index == 100) %>% hyper_index()
  
  expect_that(ind1$count, equals(c(1L, 2160L)))
  expect_warning(ind2 <- tidync(l3file) %>% hyper_filter(lon = index %% 100 == 0) %>% hyper_index())
  expect_that(ind2$count, equals(c(4201, 2160)))  
  
})
