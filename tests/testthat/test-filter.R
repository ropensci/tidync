context("filter")


library(dplyr)
test_that("unidata file", {
  f1 <- system.file("extdata", "unidata", "madis-hydro.nc", 
                    package = "tidync", mustWork = TRUE)

  ## dimensions without variables
  tidync(f1) %>%  activate("D5,D12") %>% 
    hyper_filter(QCcheckNum  = index < 2) %>%
    expect_s3_class("tidync")
  tidync(f1) %>% activate("D5,D12") %>% 
    hyper_filter() %>%
    expect_s3_class("tidync")

  })

test_that("indexing works", {
  skip_on_os("solaris")
  l3file <- system.file("extdata/oceandata", 
              "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc", 
              package = "tidync")
  ind0 <- tidync(l3file) %>% hyper_filter()
  expect_that(ind0$dimension$count[ind0$dimension$active], 
              equals(c(2160L, 4320L)))
  
  ind1 <- tidync(l3file) %>% hyper_filter(lon = index == 100) 
  
  expect_that(ind1$dimension$count[ind0$dimension$active], 
              equals(c(2160L, 1L)))
  expect_warning(ind2 <- tidync(l3file) %>% 
                   hyper_filter(lon = index %% 100 == 0))
  expect_that(ind2$dimension$count[ind2$dimension$active], 
              equals(c(2160, 4201)))  
  
})

test_that("sanity prevails", {
  
  f1 <- system.file("extdata", "unidata", "madis-hydro.nc", 
                    package = "tidync")
  
  expect_error(tidync(f1) %>% hyper_filter(a < 2), 
               "subexpressions must be in 'mutate' form")
})
