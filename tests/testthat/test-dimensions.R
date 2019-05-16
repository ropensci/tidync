context("test-dimensions")
argofile <- system.file("extdata/argo/MD5903593_001.nc", 
                        package = "tidync", mustWork = TRUE)
x <- tidync(argofile)

test_that("dimension matchup works", {
  expect_named(x %>% hyper_tibble(select_var = c("PRES", "PRES_QC")), 
               c("PRES", "PRES_QC",  "N_LEVELS", "N_PROF")) %>% nrow() %>% 
    expect_equal(986L)
  
  tab <- x %>% hyper_filter(N_LEVELS = N_LEVELS < 20) %>%  
    hyper_tibble(select_var = c("TEMP_ADJUSTED_QC", "NITRATE_ADJUSTED", 
                                "CHLA_ADJUSTED_ERROR"))
  expect_equal(dim(tab), c(38L, 5L))
  expect_equal(purrr::map_chr(tab, typeof), c(TEMP_ADJUSTED_QC = "character",
                                              NITRATE_ADJUSTED = "double",
                                              CHLA_ADJUSTED_ERROR = "double",
                                              N_LEVELS = "integer",
                                              N_PROF = "integer" 
                                              
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

test_that("grids are sensible", {
  expect_equal(hyper_grids(x), structure(list(grid = c("D0,D9,D11,D8", "D6,D9,D11,D8", "D7,D9,D11,D8", 
                                                       "D6,D9,D8", "D10,D8", "D1,D8", "D2,D8", "D3,D8", "D5,D8", "D6,D8", 
                                                       "D7,D8", "D9,D8", "D0", "D2", "D5", "D8"), ndims = c(4L, 4L, 
                                                                                                            4L, 3L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L), nvars = c(1L, 
                                                                                                                                                                               1L, 3L, 1L, 35L, 1L, 2L, 2L, 4L, 2L, 1L, 1L, 3L, 2L, 1L, 17L), 
                                              active = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, 
                                                         FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
                                              )), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                                                                          -16L)))
})

test_that("expanded tibble order is sane", 
          {
            
            tab <- expect_s3_class(
              
              ## note na.rm, usually missing values are dropped
              hyper_tibble(x, select_var = "BBP700", na.rm = FALSE), "tbl_df"
              
              )
            
            expect_equal(tab$N_LEVELS, rep(1:493, 2))
            expect_equal(tab$N_PROF, rep(1:2, each = 493))
            tab2 <- expect_s3_class(
              
              ## note na.rm, usually missing values are dropped
              hyper_tibble(x, select_var = "BBP700", na.rm = TRUE), "tbl_df"
              
            )
            
            expect_equal(tab2$N_LEVELS, 1:64)
            expect_equal(tab2$N_PROF, rep(2, 64))
        
            
            ar <- expect_s3_class(hyper_array(x, select_var = c("BBP700", "NITRATE")), 
                                  "tidync_data")  
            expect_named(ar, c("BBP700", "NITRATE"))
            expect_equal(dim(ar[[1]]), c(493, 2))
            ## note drop = FALSE here
            ar1 <- expect_s3_class(hyper_array(x, N_PROF = index == 2, drop = FALSE,
                                               select_var = c("BBP700", "NITRATE")), 
                                  "tidync_data")  
            expect_named(ar1, c("BBP700", "NITRATE"))
            expect_equal(dim(ar1[[1]]), c(493, 1))


            ## note drop = TRUE here
            ar2 <- expect_s3_class(hyper_array(x, N_PROF = index == 2, drop = TRUE,
                                               select_var = c("BBP700", "NITRATE")), 
                                   "tidync_data")  
            expect_named(ar2, c("BBP700", "NITRATE"))
            expect_equal(dim(ar2[[1]]), c(493))
        
            expect_equal(attr(ar2, "transforms")$N_LEVELS$N_LEVELS, 
                         1:493)
            expect_true(all(attr(ar2, "transforms")$N_LEVELS$N_LEVELS))
            
        
            expect_equal(attr(ar2, "transforms")$N_PROF$N_PROF, 
                         1:2)
            expect_equal(attr(ar2, "transforms")$N_PROF$selected, c(FALSE, TRUE))
            
            
            
                
        })


test_that("deprecation is working", {
  expect_named(hyper_transforms(x), c("N_LEVELS", "N_PROF"))
  expect_named(hyper_transforms(x, all = TRUE), 
               c("STRING32", "STRING4", "DATE_TIME", "STRING8", 
                 "N_PROF", "STRING64", "N_PARAM", "STRING2", "STRING256", 
                 "N_LEVELS", "N_CALIB"))
})