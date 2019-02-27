u <- "https://www.usgodae.org/ftp/outgoing/argo/dac/csiro/5905167/profiles/MR5905167_372.nc"
f <- basename(u)
dp <- "inst/extdata/argo"
download.file(u, file.path(dp, f), mode="wb")


library(raadfiles)
f <- argo_files()
## MD5903593_001.nc
file.copy(f$fullname[1], file.path("inst/extdata/argo", basename(f$fullname[1])))