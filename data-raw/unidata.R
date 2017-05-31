u <- "https://www.unidata.ucar.edu/software/netcdf/examples/madis-hydro.nc"
curl::curl_download(u, file.path("extdata/inst/unidata", basename(u)), mode = "wb")

u1 <- "https://www.unidata.ucar.edu/software/netcdf/examples/test_hgroups.nc"
curl::curl_download(u1, file.path("extdata/inst/unidata", basename(u1)), mode = "wb")
