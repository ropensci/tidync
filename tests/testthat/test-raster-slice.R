# context("test-raster-slice")
# 
# f1 <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
# ##raster(f1, varname = "precip3hrQCD")
# tnc <- tidync(f1)
# test_that("raster slice works", {
#   expect_equal(2 * 2, 4)
# })
