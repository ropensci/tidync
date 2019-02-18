context("test-utils")
hv <- structure(list(id = 35, name = "precip5minQCD", type = "NC_FLOAT", 
                     ndims = 2, natts = 8, dim_coord = FALSE), row.names = c(NA, 
                                                                        -1L), class = c("tbl_df", "tbl", "data.frame"))
hd <- structure(list(name = c("QCcheckNum", "recNum"), length = c(10, 
                                                                  1176), start = c(1L, 1L), count = c(10L, 1176L), id = c(5, 12
                                                                  ), unlim = c(FALSE, TRUE), coord_dim = c(FALSE, FALSE)), row.names = c(NA, 
                                                                                                                                         -2L), class = c("tbl_df", "tbl", "data.frame"))


f1 <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
tnc <- tidync(f1)
test_that("utils work", {
  expect_equal(hv, hyper_vars(tnc))
  expect_equal(hd, hyper_dims(tnc))
})
