context("lazy arrays")

library(ncdf4)

skip_if_not_installed("altarr")

l3file <- system.file("extdata/oceandata/S20080012008031.L3m_MO_CHL_chlor_a_9km.nc",
                      package = "tidync")

## small 3D NetCDF, values are their own global position so every read can
## be checked; 'offset' shifts them per file
mk_lazy_nc <- function(path, time_vals, offset = 0, nlon = 7, nlat = 5,
                       chunk = NA) {
  dim_lon  <- ncdim_def("lon", "degrees_east", seq(100, by = 2, length.out = nlon))
  dim_lat  <- ncdim_def("lat", "degrees_north", seq(-40, by = 2, length.out = nlat))
  dim_time <- ncdim_def("time", "days since 1970-01-01", time_vals, unlim = TRUE)
  var_sst <- ncvar_def("sst", "degC", list(dim_lon, dim_lat, dim_time),
                       missval = -999, prec = "double")
  var_n <- ncvar_def("count", "1", list(dim_lon, dim_lat, dim_time),
                     missval = -1L, prec = "integer")
  nc <- nc_create(path, list(var_sst, var_n), force_v4 = TRUE)
  n <- nlon * nlat * length(time_vals)
  ncvar_put(nc, var_sst, array(seq_len(n) + offset, c(nlon, nlat, length(time_vals))))
  ncvar_put(nc, var_n, array(seq_len(n) + as.integer(offset), c(nlon, nlat, length(time_vals))))
  nc_close(nc)
  path
}

test_that("lazy hyper_array reads nothing and matches eager values", {
  skip_on_cran()
  e <- tidync(l3file) |>
    hyper_array(lat = lat > -78 & lat < -20, lon = lon > 100 & lon < 171)
  l <- tidync(l3file) |>
    hyper_array(lat = lat > -78 & lat < -20, lon = lon > 100 & lon < 171,
                lazy = TRUE)
  a <- l$chlor_a
  expect_s3_class(l, "tidync_data")
  expect_true(altarr::is_altarr(a))
  expect_equal(altarr::altarr_stats(a)[["fetch_calls"]], 0)
  expect_identical(dim(a), dim(e$chlor_a))
  expect_identical(dimnames(a), dimnames(e$chlor_a))
  expect_identical(attr(l, "transforms"), attr(e, "transforms"))
  expect_equal(altarr::altarr_stats(a)[["fetch_calls"]], 0)

  set.seed(1)
  ij <- cbind(sample(dim(a)[1L], 200, TRUE), sample(dim(a)[2L], 200, TRUE))
  expect_identical(a[ij], e$chlor_a[ij])
  ## all those points in one batch
  expect_equal(altarr::altarr_stats(a)[["fetch_calls"]], 1)
  expect_identical(altarr::altarr_extract(a, 10:300, 5:80),
                   e$chlor_a[10:300, 5:80])
  expect_equal(mean(a), mean(e$chlor_a))
  withr_opt <- options(altarr.max_materialize = 1e8)
  on.exit(options(withr_opt), add = TRUE)
  expect_identical(a + 0, e$chlor_a + 0)
})

test_that("lazy chunk shape follows NetCDF chunking or blocks contiguous data", {
  expect_identical(tidync:::lazy_chunk_shape(c(44L, 22L), c(100L, 10L)),
                   c(44L, 10L))
  ## contiguous: whole fastest dims, split along the slowest
  op <- options(tidync.lazy.chunk_values = 100)
  on.exit(options(op), add = TRUE)
  expect_identical(tidync:::lazy_chunk_shape(NA, c(10L, 5L, 4L)),
                   c(10L, 5L, 2L))
  expect_identical(tidync:::lazy_chunk_shape(NA, c(10L, 20L, 4L)),
                   c(10L, 10L, 1L))
  expect_identical(tidync:::lazy_chunk_shape(NA, c(1000L, 3L)),
                   c(100L, 1L))
})

test_that("lazy integer, scaled and raw variables match eager", {
  skip_on_cran()
  f <- system.file("extdata/ifremer/20171002.nc", package = "tidync")
  for (raw in c(FALSE, TRUE)) {
    e <- hyper_array(f, raw_datavals = raw)
    l <- hyper_array(f, raw_datavals = raw, lazy = TRUE)
    for (v in names(e)) {
      expect_identical(typeof(l[[v]]), typeof(e[[v]]))
      expect_identical(dim(l[[v]]), dim(e[[v]]))
      expect_identical(altarr::altarr_extract(l[[v]], 1:50, 100:200),
                       e[[v]][1:50, 100:200])
    }
  }
  path <- mk_lazy_nc(tempfile(fileext = ".nc"), 1:3)
  l <- hyper_array(path, select_var = "count", lazy = TRUE)
  expect_identical(typeof(l$count), "integer")
  expect_identical(altarr::altarr_extract(l$count, , , ),
                   hyper_array(path, select_var = "count")$count)
  unlink(path)
})

test_that("lazy drop removes degenerate dimensions without reading", {
  skip_on_cran()
  path <- mk_lazy_nc(tempfile(fileext = ".nc"), 1:4)
  e <- hyper_array(path, time = index == 3, lat = index > 2)
  l <- hyper_array(path, time = index == 3, lat = index > 2, lazy = TRUE)
  expect_equal(altarr::altarr_stats(l$sst)[["fetch_calls"]], 0)
  expect_identical(dim(l$sst), dim(e$sst))
  expect_identical(dimnames(l$sst), dimnames(e$sst))
  expect_identical(altarr::altarr_extract(l$sst, , ), e$sst)
  l2 <- hyper_array(path, time = index == 3, drop = FALSE, lazy = TRUE)
  expect_identical(dim(l2$sst), c(7L, 5L, 1L))
  unlink(path)
})

test_that("lazy multi-source reads across file boundaries", {
  skip_on_cran()
  ## three files of 2, 3 and 2 time steps; chunks of 3 time steps so a
  ## chunk spans two files
  td <- tempfile("lazy-multi-")
  dir.create(td)
  steps <- list(1:2, 3:5, 6:7)
  files <- vapply(seq_along(steps), function(i) {
    mk_lazy_nc(file.path(td, sprintf("f%i.nc", i)), steps[[i]],
               offset = 1000 * i)
  }, "")
  tnc <- tidync(files, concat_dim = "time")
  op <- options(tidync.lazy.chunk_values = 7 * 5 * 3)
  on.exit(options(op), add = TRUE)
  for (sel in list(quote(index > 0), quote(index > 1), quote(index %in% c(1, 2, 6, 7)))) {
    ## (a selection that skips steps warns, as for eager reads)
    ft <- suppressWarnings(hyper_filter(tnc, time = !!sel))
    e <- suppressWarnings(hyper_array(ft))
    l <- suppressWarnings(hyper_array(ft, lazy = TRUE))
    expect_identical(dim(l$sst), dim(e$sst))
    expect_identical(dimnames(l$sst), dimnames(e$sst))
    expect_identical(altarr::altarr_extract(l$sst, , , ), e$sst)
    expect_identical(altarr::altarr_extract(l$count, , , ), e$count)
    pts <- cbind(c(1, 7, 3), c(1, 5, 2), c(1, dim(e$sst)[3L], 2))
    expect_identical(l$sst[pts], e$sst[pts])
  }
  unlink(td, recursive = TRUE)
})

test_that("lazy arrays save as a recipe and read back", {
  skip_on_cran()
  path <- mk_lazy_nc(tempfile(fileext = ".nc"), 1:3)
  l <- hyper_array(path, lazy = TRUE)
  rds <- tempfile(fileext = ".rds")
  saveRDS(l$sst, rds)
  expect_lt(file.size(rds), 5000)
  a <- readRDS(rds)
  expect_true(altarr::is_altarr(a))
  expect_identical(altarr::altarr_extract(a, , , ), hyper_array(path)$sst)
  unlink(c(path, rds))
})

test_that("lazy refuses character variables", {
  skip_on_cran()
  argofile <- system.file("extdata/argo/MD5903593_001.nc", package = "tidync")
  expect_error(tidync(argofile) |> activate("PRES_QC") |>
                 hyper_array(select_var = "PRES_QC", lazy = TRUE),
               "numeric variables only")
})

test_that("lazy multi-source batches read in parallel with mirai daemons", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  td <- tempfile("lazy-mirai-")
  dir.create(td)
  files <- vapply(1:4, function(i) {
    mk_lazy_nc(file.path(td, sprintf("f%i.nc", i)), i, offset = 1000 * i)
  }, "")
  tnc <- tidync(files, concat_dim = "time")
  e <- hyper_array(tnc)
  mirai::daemons(2)
  on.exit(mirai::daemons(0), add = TRUE)
  l <- hyper_array(tnc, lazy = TRUE)
  expect_identical(altarr::altarr_extract(l$sst, , , ), e$sst)
  unlink(td, recursive = TRUE)
})
