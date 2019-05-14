context("test-dimensions")
x <- tidync(system.file("extdata/argo/MD5903593_001.nc", 
                        package = "tidync", mustWork = TRUE))

test_that("dimension matchup works", {
  expect_named(x %>% hyper_tibble(select_var = c("PRES", "PRES_QC")), 
               c("PRES", "PRES_QC", "N_PROF", "N_LEVELS")) %>% nrow() %>% 
    expect_equal(986L)
  
  tab <- x %>% hyper_filter(N_LEVELS = N_LEVELS < 20) %>%  
    hyper_tibble(select_var = c("TEMP_ADJUSTED_QC", "NITRATE_ADJUSTED", 
                                "CHLA_ADJUSTED_ERROR"))
  expect_equal(dim(tab), c(38L, 5L))
  expect_equal(purrr::map_chr(tab, typeof), c(TEMP_ADJUSTED_QC = "character",
                                              NITRATE_ADJUSTED = "double",
                                              CHLA_ADJUSTED_ERROR = "double",
                                              N_PROF = "integer", 
                                              N_LEVELS = "integer"
  ))
  expect_equivalent(tab$CHLA_ADJUSTED_ERROR, 
  c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
   NA, NA, NA, NA, 1.28159999847412, 1.36080002784729, 1.20239996910095, 
   1.49759995937347, 1.20239996910095, 1.28159999847412, 1.28159999847412, 
   1.28159999847412, 1.64160001277924, 1.23839998245239, 1.28159999847412, 
   1.33920001983643, 1.21679997444153, 1.15919995307922, 1.07280004024506, 
   1.00080001354218, 0.986400008201599, 0.943199992179871, 
   1.38960003852844
  ))
})

test_that("deprecation is working", {
  expect_named(hyper_transforms(x), c("N_LEVELS", "N_PROF"))
  expect_named(hyper_transforms(x, all = TRUE), 
               c("STRING32", "STRING4", "DATE_TIME", "STRING8", 
                 "N_PROF", "STRING64", "N_PARAM", "STRING2", "STRING256", 
                 "N_LEVELS", "N_CALIB"))
})