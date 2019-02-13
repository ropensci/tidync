u <- "https://www.usgodae.org/ftp/outgoing/argo/dac/csiro/5905167/profiles/MR5905167_372.nc"
f <- basename(u)
dp <- "inst/extdata/argo"
download.file(u, file.path(dp, f), mode="wb")
