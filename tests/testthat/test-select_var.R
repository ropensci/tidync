context("test-select_var")

test_that("select_var is robust", {
  f <- system.file("extdata/argo/MR5905167_372.nc", 
                   package = "tidync", mustWork = TRUE)
  expect_warning(
    tidync(f) %>% 
    hyper_tibble(select_var = c("PRES", "TEMP", "PSAL", 
                                "DOXY", "CHLA", "BBP700", 
                                "NITRATE", "TEDDYBEAR")), 
    "some select_var variables not found, and ignored:"
  )
  expect_silent(
    tidync(f) %>% 
      hyper_tibble(select_var = c("PRES", "TEMP", "PSAL", 
                                  "DOXY", "CHLA")))
  
  expect_error(    tidync(f) %>% 
                     hyper_tibble(select_var = c("ABC", "WHO", "IS"))
                     )
})
