if (FALSE) {
  context("speed")
library(raster)
u <- "ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.wkmean.1990-present.nc"
tfile <- file.path(tempdir(), basename(u))
if (!file.exists(tfile)) curl::curl_download(u, tfile, mode = "wb")


# in-development pattern for tidync
## but generic version would deal with raster, ff, hdf, etc.
## dispatching for raster file backing as nec

raw_fun <- function(file, x) {
  ncdf4::ncvar_get(ncdf4::nc_open(file), x$variable[1], 
                   start = x$start, count = x$count) %>% make_degen()
}
no_ones <- function(x) {
  x[x > 1]
}
make_degen <- function(x) {
  if (length(dim(x)) == 2) x <- array(x, c(dim(x), 1L))
  x
}

hyper_read  <- function(filename, ...) {
  #if (missing(xyex)) xyex <- c(-180, 180, -90, 90)
  hf <- tidync(filename[1]) %>% 
    hyper_filter(...)
  hi <- hf %>% hyper_index()
  raster_extent <- raster::extent(unlist(lapply(1:2, function(x) range(hf[[x]][[names(hf)[x]]]))))
  
#  raster_extent <- raster::extent(range(hf$lon$lon) + c(-0.125, 0.125), range(hf$lat$lat)+ c(-0.125, 0.125))
  if (length(filename) > 1L) {
    setExtent(brick(array(unlist(lapply(filename, function(ifile) raw_fun(ifile, x = hi))), c(no_ones(hi$count), length(filename)))[, rev(seq(nrow(hf$lat))), ],
                    crs = "+proj=longlat +datum=WGS84 +no_defs"),
              raster_extent)
  } else {
    setExtent(brick(raw_fun(filename, x = hi),
                     crs = "+proj=longlat +datum=WGS84 +no_defs"),
              raster_extent)
  }
}
xyex <- extent(100, 160, -30, 60)
library(dplyr)
test_that("tidync is faster", {

  ## all xy, few time slices
 (hr0_time <- system.time({hr <- hyper_read(tfile, time = between(step, 50, 60))}))
 (rs0_time <- system.time({rs <- raster::subset(brick(tfile), 50:60)}))
  
  ## few time slices
  (hr_time <- system.time({hr <- hyper_read(tfile, lon = between(lon, 100, 160), lat = between(lat, -30, 60), time = between(step, 50, 60))}))
  (rs_time <- system.time({rs <- crop(raster::subset(brick(tfile), 50:60), xyex)}))
  
  
  ## many time slices
  (hr1_time <- system.time({hr <- hyper_read(tfile, lon = between(lon, 100, 160), lat = between(lat, -30, 60), time = between(step, 50, 460))}))
  (rs1_time <- system.time({rs <- crop(raster::subset(brick(tfile), 50:460), xyex)}))
  (rs1_time <- system.time({rs <- raster::subset(crop(brick(tfile), xyex), 50:460)}))
  
  
  ## arbitrary time slices
  time_step <- sort(sample(1:1430, 100))
  (hr2_time <- system.time({hr <- hyper_read(tfile, lon = between(lon, 100, 160), lat = between(lat, -30, 60), time = step %in% time_step)}))
  (rs2_time <- system.time({rs <- crop(raster::subset(brick(tfile), time_step), xyex)}))
  
  
})
}