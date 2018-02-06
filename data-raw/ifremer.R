#f <- file.path("/rdsi/PUBLIC/raad", "data/ftp.ifremer.fr/ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/2013/20130415.nc")
#file.copy(f, file.path("inst", "extdata", "ifremer", basename(f)))

u <- "ftp://ftp.ifremer.fr/ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/2017/20171002.nc.Z"
download.file(u, file.path("data-raw", basename(u)), mode = "wb")
system(sprintf("uncompress %s", file.path("data-raw", basename(u))))
file.copy(file.path("data-raw", gsub(".Z", "", basename(u))), 
          file.path("inst/extdata/ifremer", gsub(".Z", "", basename(u))))
