f <- file.path("/rdsi/PUBLIC/raad", "data/ftp.ifremer.fr/ifremer/cersat/products/gridded/psi-concentration/data/antarctic/daily/netcdf/2013/20130415.nc")
file.copy(f, file.path("inst", "extdata", "ifremer", basename(f)))
