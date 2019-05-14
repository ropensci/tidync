context("test-lib-version")

test_that("robust to version capabilites", {
  skip_on_os("solaris")
  ufile <- system.file("extdata", "unidata", "test_hgroups.nc", 
    package = "tidync", mustWork = TRUE)
  ## some versions of NetCDF 4.. don't support this file
  ## (4.1.3 tidync/issues/82)
  group_nc <- try(tidync(ufile), silent = TRUE)
  if (inherits(group_nc, "try-error")) {
    expect_true(grepl("NetCDF: Invalid dimension ID or name", unclass(group_nc)[1]))
  } else {
    expect_s3_class(group_nc, "tidync")
  }
  })
