context("multi-source")

library(ncdf4)

## Helper: create a small NetCDF file with lon, lat, time, and one variable
make_test_nc <- function(path, time_vals, sst_data = NULL) {
  lon_vals <- seq(100, 110, by = 2)  # 6 lons

lat_vals <- seq(-40, -30, by = 2) # 6 lats
  nlon <- length(lon_vals)
  nlat <- length(lat_vals)
  ntime <- length(time_vals)

  dim_lon  <- ncdim_def("lon", "degrees_east", lon_vals)
  dim_lat  <- ncdim_def("lat", "degrees_north", lat_vals)
  dim_time <- ncdim_def("time", "days since 1970-01-01", time_vals, unlim = TRUE)

  var_sst <- ncvar_def("sst", "degC", list(dim_lon, dim_lat, dim_time),
                        missval = -999)

  nc <- nc_create(path, list(var_sst))

  if (is.null(sst_data)) {
    sst_data <- array(rnorm(nlon * nlat * ntime, mean = 15),
                      dim = c(nlon, nlat, ntime))
  }
  ncvar_put(nc, var_sst, sst_data)
  nc_close(nc)
  path
}

test_that("multi-source basic construction works", {
  skip_on_cran()
  td <- tempdir()
  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_2020_%02d.nc", i))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262)  # one time step each
    f
  }, character(1))

  tnc <- tidync(files, concat_dim = "time")

  expect_s3_class(tnc, "tidync")
  expect_equal(nrow(tnc$source), 3L)
  expect_equal(tnc$concat_dim, "time")

  # The time transform should have 3 rows (one per source)
  time_trans <- tnc$transforms[["time"]]
  expect_equal(nrow(time_trans), 3L)
  expect_true(all(time_trans$selected))
  expect_equal(time_trans$source_id, 1:3)
  expect_equal(time_trans$index, 1:3)
  expect_equal(time_trans$local_index, c(1L, 1L, 1L))

  # Shared dims should be unchanged
  expect_equal(nrow(tnc$transforms[["lon"]]), 6L)
  expect_equal(nrow(tnc$transforms[["lat"]]), 6L)

  # Dimension table should reflect total
  expect_equal(tnc$dimension$length[tnc$dimension$name == "time"], 3L)

  unlink(files)
})

test_that("multi-source hyper_filter works", {
  skip_on_cran()
  td <- tempdir()
  files <- vapply(1:4, function(i) {
    f <- file.path(td, sprintf("sst_filt_%02d.nc", i))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262)
    f
  }, character(1))

  tnc <- tidync(files, concat_dim = "time")

  # Filter to only the last 2 time steps
  filtered <- tnc |> hyper_filter(time = index > 2)
  time_trans <- filtered$transforms[["time"]]
  expect_equal(sum(time_trans$selected), 2L)
  expect_equal(which(time_trans$selected), 3:4)

  # Filter on shared dim too
  filtered2 <- tnc |> hyper_filter(time = index > 2, lon = lon < 106)
  expect_equal(sum(filtered2$transforms[["time"]]$selected), 2L)
  expect_equal(sum(filtered2$transforms[["lon"]]$selected), 3L)

  unlink(files)
})

test_that("multi-source hyper_array reads correctly", {
  skip_on_cran()
  td <- tempdir()

  # Create 3 files with known data
  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_read_%02d.nc", i))
    # Fill with value = i so we can verify which file was read
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  tnc <- tidync(files, concat_dim = "time")

  # Read all
  arr <- hyper_array(tnc)
  expect_equal(dim(arr$sst), c(6, 6, 3))
  # First time slice should be all 1s, second all 2s, etc.
  expect_true(all(arr$sst[,,1] == 1))
  expect_true(all(arr$sst[,,2] == 2))
  expect_true(all(arr$sst[,,3] == 3))

  # Read with filter — only file 2
  arr2 <- tnc |>
    hyper_filter(time = index == 2) |>
    hyper_array()
  expect_equal(dim(arr2$sst), c(6, 6))  # degenerate dim dropped
  expect_true(all(arr2$sst == 2))

  unlink(files)
})

test_that("multi-source hyper_tibble works", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:2, function(i) {
    f <- file.path(td, sprintf("sst_tib_%02d.nc", i))
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  tnc <- tidync(files, concat_dim = "time")
  tib <- hyper_tibble(tnc, na.rm = FALSE)

  expect_s3_class(tib, "tbl_df")
  # 6 * 6 * 2 = 72 rows
  expect_equal(nrow(tib), 72L)
  expect_true("sst" %in% names(tib))
  expect_true("time" %in% names(tib))
  expect_true("lon" %in% names(tib))
  expect_true("lat" %in% names(tib))

  # Values from file 1 and file 2
  expect_true(all(tib$sst %in% c(1, 2)))

  unlink(files)
})

test_that("multi-source filter selects only needed sources", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_sel_%02d.nc", i))
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  tnc <- tidync(files, concat_dim = "time")

  # Filter to only the middle file
  arr <- tnc |>
    hyper_filter(time = index == 2) |>
    hyper_array()

  # Should have read only from file 2
  expect_true(all(arr$sst == 2))

  unlink(files)
})

test_that("single file with concat_dim works (degenerate case)", {
  skip_on_cran()
  td <- tempdir()
  f <- file.path(td, "sst_single.nc")
  make_test_nc(f, time_vals = 18262)

  tnc <- tidync(f, concat_dim = "time")
  expect_equal(nrow(tnc$source), 1L)
  expect_equal(tnc$concat_dim, "time")

  arr <- hyper_array(tnc)
  expect_equal(dim(arr$sst), c(6, 6))

  unlink(f)
})

test_that("fast mode works with conforming files", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_fast_%02d.nc", i))
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  tnc <- tidync(files, concat_dim = "time", fast = TRUE)
  expect_true(tnc$fast_mode)
  expect_equal(nrow(tnc$source), 3L)

  # Reading should succeed and produce correct data
  arr <- hyper_array(tnc)
  expect_equal(dim(arr$sst), c(6, 6, 3))
  expect_true(all(arr$sst[,,2] == 2))

  unlink(files)
})

test_that("fast mode detects mismatched shared dimension at read time", {
  skip_on_cran()
  td <- tempdir()

  # File 1: 6 lons
  f1 <- file.path(td, "sst_mismatch_01.nc")
  make_test_nc(f1, time_vals = 18262)

  # File 2: different lon count (create manually)
  f2 <- file.path(td, "sst_mismatch_02.nc")
  lon2 <- seq(100, 108, by = 2)  # only 5 lons!
  lat2 <- seq(-40, -30, by = 2)
  dim_lon2  <- ncdim_def("lon", "degrees_east", lon2)
  dim_lat2  <- ncdim_def("lat", "degrees_north", lat2)
  dim_time2 <- ncdim_def("time", "days since 1970-01-01", 18292, unlim = TRUE)
  var_sst2 <- ncvar_def("sst", "degC", list(dim_lon2, dim_lat2, dim_time2),
                         missval = -999)
  nc2 <- nc_create(f2, list(var_sst2))
  ncvar_put(nc2, var_sst2, array(99, dim = c(5, 6, 1)))
  nc_close(nc2)

  # Fast mode: construction succeeds (no validation of shared dims)
  tnc <- tidync(c(f1, f2), concat_dim = "time", fast = TRUE)
  expect_s3_class(tnc, "tidync")

  # But reading should fail with a clear message
  expect_error(hyper_array(tnc), "fast mode.*dimension.*lon.*length")

  unlink(c(f1, f2))
})

test_that("non-fast mode detects mismatched dimension at construction", {
  skip_on_cran()
  td <- tempdir()

  f1 <- file.path(td, "sst_val_01.nc")
  make_test_nc(f1, time_vals = 18262)

  f2 <- file.path(td, "sst_val_02.nc")
  lon2 <- seq(100, 108, by = 2)
  lat2 <- seq(-40, -30, by = 2)
  dim_lon2  <- ncdim_def("lon", "degrees_east", lon2)
  dim_lat2  <- ncdim_def("lat", "degrees_north", lat2)
  dim_time2 <- ncdim_def("time", "days since 1970-01-01", 18292, unlim = TRUE)
  var_sst2 <- ncvar_def("sst", "degC", list(dim_lon2, dim_lat2, dim_time2),
                         missval = -999)
  nc2 <- nc_create(f2, list(var_sst2))
  ncvar_put(nc2, var_sst2, array(99, dim = c(5, 6, 1)))
  nc_close(nc2)

  # Non-fast mode: construction should fail
  expect_error(tidync(c(f1, f2), concat_dim = "time"),
               "dimension.*lon.*length")

  unlink(c(f1, f2))
})

test_that("invalid concat_dim errors at construction", {
  skip_on_cran()
  td <- tempdir()
  f <- file.path(td, "sst_badcd.nc")
  make_test_nc(f, time_vals = 18262)

  expect_error(tidync(c(f, f), concat_dim = "nonexistent"),
               "concat_dim.*not found")

  unlink(f)
})

test_that("multi-step files concatenate correctly", {
  skip_on_cran()
  td <- tempdir()

  # File 1: 3 time steps, File 2: 2 time steps
  f1 <- file.path(td, "sst_multi_01.nc")
  make_test_nc(f1, time_vals = c(18262, 18263, 18264),
               sst_data = array(1, dim = c(6, 6, 3)))

  f2 <- file.path(td, "sst_multi_02.nc")
  make_test_nc(f2, time_vals = c(18265, 18266),
               sst_data = array(2, dim = c(6, 6, 2)))

  tnc <- tidync(c(f1, f2), concat_dim = "time")

  time_trans <- tnc$transforms[["time"]]
  expect_equal(nrow(time_trans), 5L)
  expect_equal(time_trans$source_id, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(time_trans$local_index, c(1L, 2L, 3L, 1L, 2L))
  expect_equal(time_trans$index, 1:5)

  arr <- hyper_array(tnc, drop = FALSE)
  expect_equal(dim(arr$sst), c(6, 6, 5))
  expect_true(all(arr$sst[,,1:3] == 1))
  expect_true(all(arr$sst[,,4:5] == 2))

  # Filter to span both files
  arr2 <- tnc |>
    hyper_filter(time = index >= 3 & index <= 4) |>
    hyper_array(drop = FALSE)
  expect_equal(dim(arr2$sst), c(6, 6, 2))
  expect_true(all(arr2$sst[,,1] == 1))  # from file 1, step 3
  expect_true(all(arr2$sst[,,2] == 2))  # from file 2, step 1

  unlink(c(f1, f2))
})

test_that("print method shows multi-source info", {
  skip_on_cran()
  td <- tempdir()
  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_print_%02d.nc", i))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262)
    f
  }, character(1))

  tnc <- tidync(files, concat_dim = "time")
  out <- capture.output(print(tnc))
  expect_true(any(grepl("Concatenated along 'time'", out)))
  expect_true(any(grepl("3 sources", out)))

  unlink(files)
})

## ---- Values-supplied path tests ----

test_that("values-supplied path with Date works", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_vals_%02d.nc", i))
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  dates <- as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
  tnc <- tidync(files, concat_dim = list(name = "time", values = dates))

  expect_s3_class(tnc, "tidync")
  expect_equal(nrow(tnc$source), 3L)
  expect_equal(tnc$concat_dim, "time")

  # The time column should hold Date values
  time_trans <- tnc$transforms[["time"]]
  expect_equal(nrow(time_trans), 3L)
  expect_s3_class(time_trans$time, "Date")
  expect_equal(time_trans$time, dates)
  expect_true("timestamp" %in% names(time_trans))

  # Filter on Date values
  filtered <- tnc |> hyper_filter(time = time > as.Date("2020-01-15"))
  expect_equal(sum(filtered$transforms[["time"]]$selected), 2L)

  # Read should work
  arr <- hyper_array(tnc, drop = FALSE)
  expect_equal(dim(arr$sst), c(6, 6, 3))

  unlink(files)
})

test_that("values-supplied path with POSIXct works", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_posix_%02d.nc", i))
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  times <- as.POSIXct(c("2020-01-01 12:00", "2020-02-01 12:00",
                         "2020-03-01 12:00"), tz = "UTC")
  tnc <- tidync(files, concat_dim = list(name = "time", values = times))

  time_trans <- tnc$transforms[["time"]]
  expect_s3_class(time_trans$time, "POSIXct")
  expect_true("timestamp" %in% names(time_trans))

  # Filter on POSIXct
  filtered <- tnc |>
    hyper_filter(time = time > as.POSIXct("2020-01-15", tz = "UTC"))
  expect_equal(sum(filtered$transforms[["time"]]$selected), 2L)

  unlink(files)
})

test_that("values-supplied path with numeric works", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_numvals_%02d.nc", i))
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  vals <- c(100, 200, 300)
  tnc <- tidync(files, concat_dim = list(name = "time", values = vals))

  time_trans <- tnc$transforms[["time"]]
  expect_equal(time_trans$time, vals)

  # Filter on numeric values
  filtered <- tnc |> hyper_filter(time = time > 150)
  expect_equal(sum(filtered$transforms[["time"]]$selected), 2L)

  arr <- hyper_array(filtered, drop = FALSE)
  expect_equal(dim(arr$sst), c(6, 6, 2))
  expect_true(all(arr$sst[,,1] == 2))
  expect_true(all(arr$sst[,,2] == 3))

  unlink(files)
})

test_that("values-supplied path errors on length mismatch", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:3, function(i) {
    f <- file.path(td, sprintf("sst_mismatch_vals_%02d.nc", i))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262)
    f
  }, character(1))

  dates <- as.Date(c("2020-01-01", "2020-02-01"))  # only 2, need 3
  expect_error(
    tidync(files, concat_dim = list(name = "time", values = dates)),
    "must match"
  )

  unlink(files)
})

test_that("list concat_dim without values falls through to file-reading", {
  skip_on_cran()
  td <- tempdir()

  files <- vapply(1:2, function(i) {
    f <- file.path(td, sprintf("sst_listnovals_%02d.nc", i))
    data <- array(as.double(i), dim = c(6, 6, 1))
    make_test_nc(f, time_vals = (i - 1) * 30 + 18262, sst_data = data)
    f
  }, character(1))

  # list with name only, no values
  tnc <- tidync(files, concat_dim = list(name = "time"))
  expect_equal(nrow(tnc$source), 2L)

  # Should still work (reads from files)
  arr <- hyper_array(tnc, drop = FALSE)
  expect_equal(dim(arr$sst), c(6, 6, 2))

  unlink(files)
})

test_that("list concat_dim errors without name element", {
  skip_on_cran()
  td <- tempdir()
  f <- file.path(td, "sst_noname.nc")
  make_test_nc(f, time_vals = 18262)

  expect_error(
    tidync(c(f, f), concat_dim = list(values = 1:2)),
    "must have a 'name' element"
  )

  unlink(f)
})
