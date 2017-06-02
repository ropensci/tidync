raw_fun <- function(file, x) {
  ncdf4::ncvar_get(ncdf4::nc_open(file), x$variable[1], 
                   start = x$start, count = x$count)
}
no_ones <- function(x) {
  x[x > 1]
}
hyper_fasst <- function(filename, xyex, ...) {
  if (missing(xyex)) xyex <- c(-180, 180, -90, 90)
  hf <- tidync(filename[1]) %>% 
    hyper_filter(lon = dplyr::between(lon, xyex[1], xyex[2]),
                 lat = dplyr::between(lat, xyex[3], xyex[4]))
  hi <- hf %>% hyper_index()
  raster_extent <- raster::extent(range(hf$lon$lon) + c(-0.125, 0.125), range(hf$lat$lat)+ c(-0.125, 0.125))
  if (length(filename) > 1L) {
  setExtent(brick(array(unlist(lapply(filename, function(ifile) raw_fun(ifile, x = hi))), c(no_ones(hi$count), length(filename)))[, rev(seq(nrow(hf$lat))), ],
            crs = "+proj=longlat +datum=WGS84 +no_defs"),
            raster_extent)
  } else {
    setExtent(raster(array(unlist(lapply(filename, function(ifile) raw_fun(ifile, x = hi))), c(no_ones(hi$count)))[, rev(seq(nrow(hf$lat)))],
                    crs = "+proj=longlat +datum=WGS84 +no_defs"),
              raster_extent)
  }
}


library(tidync)
library(raadtools)

sstf <- ghrsstfiles()
files <- sstf[1:30, ]

hyper_fasst(files$fullname[1], extent(145, 146, -45, -44))
xyex<- c(130, 226, -70, -20)
library(rbenchmark)
benchmark(raad_1 = readsst(files$date[1], inputfiles = sstf, lon180 = FALSE, xylim = extent(xyex)), 
          fasst_1 = hyper_fasst(files$fullname[1], xyex),
          replications = 100)

## raadtools is faster for single file slurps
# test replications elapsed relative user.self sys.self user.child sys.child
# 2 fasst_1          100  30.155    1.544    19.772    0.388          0         0
# 1  raad_1          100  19.535    1.000     6.192    0.268          0         0

## tidync wins for multiple file 
benchmark(raad_b =  readsst(files$date, inputfiles = sstf, lon180 = FALSE, xylim = extent(xyex)), 
          fasst_b =  hyper_fasst(files$fullname, xyex), 
          replications = 12)
# test replications elapsed relative user.self sys.self user.child sys.child
# 2 fasst_b           12  30.459    1.000     9.224    1.040          0         0
# 1  raad_b           12  69.121    2.269    21.584    1.136          0         0