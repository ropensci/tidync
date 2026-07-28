context("multi-source regressions")

library(ncdf4)

## Helper: small NetCDF with lon, lat, time and one variable, constant fill
## value per file so provenance of every value read can be asserted.
mk_nc <- function(path, time_vals, fill = 1, nlon = 6, nlat = 6) {
  dim_lon  <- ncdim_def("lon", "degrees_east",
                        seq(100, by = 2, length.out = nlon))
  dim_lat  <- ncdim_def("lat", "degrees_north",
                        seq(-40, by = 2, length.out = nlat))
  dim_time <- ncdim_def("time", "days since 1970-01-01", time_vals,
                        unlim = TRUE)
  var_sst <- ncvar_def("sst", "degC", list(dim_lon, dim_lat, dim_time),
                       missval = -999)
  nc <- nc_create(path, list(var_sst))
  ncvar_put(nc, var_sst,
            array(as.double(fill), c(nlon, nlat, length(time_vals))))
  nc_close(nc)
  path
}

mk_set <- function(n, prefix, steps = 1) {
  vapply(seq_len(n), function(i) {
    tv <- 18262 + ((i - 1) * steps + seq_len(steps) - 1) * 30
    mk_nc(file.path(tempdir(), sprintf("%s_%02i.nc", prefix, i)), tv, fill = i)
  }, character(1))
}

test_that("single file with concat_dim is a real degenerate multi-source", {
  skip_on_cran()
  f <- mk_set(1, "reg_degen")
  tnc <- tidync(f, concat_dim = "time")
  ## concat_dim must not be silently dropped
  expect_equal(tnc$concat_dim, "time")
  expect_equal(nrow(tnc$source), 1L)
  expect_true(all(c("source_id", "local_index") %in%
                    names(tnc$transforms$time)))
  ## and the read path must work end to end
  arr <- hyper_array(tnc, drop = FALSE)
  expect_equal(dim(arr$sst), c(6L, 6L, 1L))
  tib <- hyper_tibble(tnc, na.rm = FALSE)
  expect_equal(nrow(tib), 36L)
  unlink(f)
})

test_that("fast mode never yields NA time values in hyper_tibble", {
  skip_on_cran()
  f <- mk_set(4, "reg_fastna")
  tnc <- tidync(f, concat_dim = "time", fast = TRUE)
  ## fast mode cannot build per-source CFtime timestamps; the column must be
  ## absent rather than partially NA (a partial column poisons dimnames and
  ## the tibble time column for sources 2..N)
  expect_false("timestamp" %in% names(tnc$transforms$time))
  tib <- hyper_tibble(tnc, na.rm = FALSE)
  expect_false(anyNA(tib$time))
  expect_type(tib$time, "double")
  expect_equal(sort(unique(tib$time)), 18262 + (0:3) * 30)
  unlink(f)
})

test_that("fast mode and full mode read identical data", {
  skip_on_cran()
  f <- mk_set(4, "reg_equiv")
  a_full <- hyper_array(tidync(f, concat_dim = "time"), drop = FALSE)
  a_fast <- hyper_array(tidync(f, concat_dim = "time", fast = TRUE),
                        drop = FALSE)
  expect_equal(unclass(a_full$sst), unclass(a_fast$sst),
               check.attributes = FALSE)
  unlink(f)
})

test_that("multi-source equals a single concatenated file", {
  skip_on_cran()
  ## gold standard: 4 single-step files vs one 4-step file must produce
  ## identical results for the same filters
  f_multi <- mk_set(4, "reg_gold")
  tv <- 18262 + (0:3) * 30
  f_one <- mk_nc(file.path(tempdir(), "reg_gold_all.nc"), tv)
  ## put the per-file fill values into the single file so data match
  nc <- nc_open(f_one, write = TRUE)
  for (i in 1:4) ncvar_put(nc, "sst", array(as.double(i), c(6, 6, 1)),
                           start = c(1, 1, i), count = c(6, 6, 1))
  nc_close(nc)

  tm <- tidync(f_multi, concat_dim = "time")
  t1 <- tidync(f_one)

  for (filt in list(quote(index > 1), quote(time >= 18292 & time <= 18322))) {
    am <- hyper_array(hyper_filter(tm, time = !!filt), drop = FALSE)
    a1 <- hyper_array(hyper_filter(t1, time = !!filt), drop = FALSE)
    expect_equal(unclass(am$sst), unclass(a1$sst), check.attributes = FALSE)
  }
  bm <- hyper_tibble(tm, lon = lon < 106, na.rm = FALSE)
  b1 <- hyper_tibble(t1, lon = lon < 106, na.rm = FALSE)
  expect_equal(bm$sst, b1$sst)
  expect_equal(bm$time, b1$time)
  unlink(c(f_multi, f_one))
})

test_that("values-supplied path validates at read time", {
  skip_on_cran()
  ## mismatched shared dimension: friendly error, not a raw C error
  f_ok  <- mk_set(1, "reg_valshared")
  f_bad <- mk_nc(file.path(tempdir(), "reg_valshared_bad.nc"), 999,
                 fill = 9, nlon = 5)
  tnc <- tidync(c(f_ok, f_bad),
                concat_dim = list(name = "time", values = c(1, 2)))
  expect_error(hyper_array(tnc), "dimension 'lon' has length 5")

  ## multi-step files with one value per file: error, not a silent subset
  fm1 <- mk_nc(file.path(tempdir(), "reg_valmulti_1.nc"),
               c(100, 101, 102), fill = 1)
  fm2 <- mk_nc(file.path(tempdir(), "reg_valmulti_2.nc"),
               c(103, 104), fill = 2)
  tnc2 <- tidync(c(fm1, fm2),
                 concat_dim = list(name = "time",
                                   values = as.Date(c("2020-01-01",
                                                      "2020-02-01"))))
  expect_error(hyper_array(tnc2), "concat dimension 'time' has length 3")
  unlink(c(f_ok, f_bad, fm1, fm2))
})

test_that("non-contiguous selection that skips whole sources works", {
  skip_on_cran()
  f <- mk_set(4, "reg_skip")
  tnc <- tidync(f, concat_dim = "time")
  arr <- suppressWarnings(
    hyper_filter(tnc, time = index %in% c(1, 4)) |> hyper_array(drop = FALSE)
  )
  ## the across-file case reads only the two needed files and is tight
  expect_equal(dim(arr$sst), c(6L, 6L, 2L))
  expect_equal(sort(unique(as.vector(arr$sst))), c(1, 4))
  unlink(f)
})

test_that("non-contiguous selection within one source matches single-source", {
  skip_on_cran()
  ## known limitation shared with single-source tidync: within-file
  ## non-contiguous selection warns then fails at hyper_array (dimnames
  ## versus bounding-slab extent). Lock in parity so a future fix updates
  ## both paths together (see also single-source behaviour on main).
  fm1 <- mk_nc(file.path(tempdir(), "reg_within_1.nc"), c(100, 101, 102))
  fm2 <- mk_nc(file.path(tempdir(), "reg_within_2.nc"), c(103, 104))
  tnc <- tidync(c(fm1, fm2), concat_dim = "time")
  expect_error(suppressWarnings(
    hyper_filter(tnc, time = index %in% c(1, 3)) |> hyper_array()
  ))
  unlink(c(fm1, fm2))
})

test_that("unsorted sources keep file order and value filters still work", {
  skip_on_cran()
  f1 <- mk_nc(file.path(tempdir(), "reg_unsort_1.nc"), 200, fill = 1)
  f2 <- mk_nc(file.path(tempdir(), "reg_unsort_2.nc"), 100, fill = 2)
  tnc <- tidync(c(f1, f2), concat_dim = "time")
  expect_equal(tnc$transforms$time$time, c(200, 100))
  arr <- hyper_filter(tnc, time = time < 150) |> hyper_array(drop = FALSE)
  expect_true(all(arr$sst == 2))
  unlink(c(f1, f2))
})

test_that("tidync_data roundtrip preserves multi-source", {
  skip_on_cran()
  f <- mk_set(3, "reg_round")
  tnc <- tidync(f, concat_dim = "time")
  a <- hyper_array(tnc)
  t2 <- tidync(a)
  expect_equal(t2$concat_dim, "time")
  expect_equal(nrow(t2$source), 3L)
  unlink(f)
})

test_that("what= combines with concat_dim", {
  skip_on_cran()
  f <- mk_set(2, "reg_what")
  tnc <- tidync(f, what = "sst", concat_dim = "time")
  expect_equal(tnc$concat_dim, "time")
  expect_equal(dim(hyper_array(tnc, drop = FALSE)$sst), c(6L, 6L, 2L))
  unlink(f)
})

test_that("hyper_tbl_cube spans all sources", {
  skip_on_cran()
  f <- mk_set(4, "reg_cube")
  cube <- hyper_tbl_cube(tidync(f, concat_dim = "time"))
  expect_equal(lengths(cube$dims), c(lon = 6L, lat = 6L, time = 4L))
  expect_equal(dim(cube$mets[[1]]), c(6L, 6L, 4L))
  unlink(f)
})

test_that("parallel reads via mirai daemons match sequential", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  f <- mk_set(4, "reg_mirai")
  tnc <- tidync(f, concat_dim = "time")
  a_seq <- hyper_array(tnc, drop = FALSE)
  mirai::daemons(2)
  on.exit(mirai::daemons(0), add = TRUE)
  a_par <- hyper_array(tnc, drop = FALSE)
  expect_equal(unclass(a_seq$sst), unclass(a_par$sst),
               check.attributes = FALSE)
  ## validation errors must propagate out of daemons
  f_bad <- mk_nc(file.path(tempdir(), "reg_mirai_bad.nc"), 999,
                 fill = 9, nlon = 5)
  tbad <- tidync(c(f[1], f_bad), concat_dim = "time", fast = TRUE)
  expect_error(hyper_array(tbad), "dimension 'lon'")
  unlink(c(f, f_bad))
})
