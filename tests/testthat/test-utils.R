context("test-utils")
hv <- structure(
  list(
    id = c(35, 42, 49, 56, 63, 70, 77),
    name = c(
      "precip5minQCD",
      "precip1hrQCD",
      "precip3hrQCD",
      "precip6hrQCD",
      "precip12hrQCD",
      "precip24hrQCD",
      "precipAccumQCD"
    ),
    type = c(
      "NC_FLOAT",
      "NC_FLOAT",
      "NC_FLOAT",
      "NC_FLOAT",
      "NC_FLOAT",
      "NC_FLOAT",
      "NC_FLOAT"
    ),
    ndims = c(2, 2, 2, 2, 2, 2, 2),
    natts = c(8, 8, 8, 8, 8,
              8, 8),
    dim_coord = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                  FALSE)
  ),
  row.names = c(NA,-7L),
  class = c("tbl_df", "tbl",
            "data.frame"))
hd <-
  structure(
    list(
      name = c("QCcheckNum", "recNum"),
      length = c(10,
                 1176),
      start = c(1L, 1L),
      count = c(10L, 1176L),
      id = c(5, 12),
      unlim = c(FALSE, TRUE),
      coord_dim = c(FALSE, FALSE)
    ),
    row.names = c(NA,-2L),
    class = c("tbl_df", "tbl", "data.frame")
  )

f1 <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
tnc <- tidync(f1)
test_that("utils work", {
  expect_equal(names(hv), names(hyper_vars(tnc)))
  expect_equal(dim(hd), dim(hyper_dims(tnc)))
})
