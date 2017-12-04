## read all shapefiles, we
library(sf)
library(dplyr)
library(raster)
library(tabularaster)
## tidync@expt-group-by
devtools::load_all(".")


bom <- "/rdsi/public/CFA/BoM_daily_vars/tmax/bom-tmax_day-19610101-19610131.nc"
tidync(bom) %>% activate(tmax_day)
shp <-read_sf("/mnt/WineAustralia_working_files/shapes/Wine-Regions-GI-Regions/individual_shp/region_tasmania.shp")
system.time(d <- tidync(bom) %>% activate(tmax_day) %>% hyper_group_by(shp) %>% hyper_tibble())

library(ggplot2)

bfiles <- raadfiles:::bom_tmax_daily_files()

ggplot(d, aes(longitude, latitude, fill = tmax_day)) + geom_raster() +  facet_wrap(~time) + coord_equal()

system.time({
  out <- vector("list", nrow(bfiles))
  for (i in seq_len(nrow(bfiles))) {
    out[[i]] <- tidync(bfiles$fullname[i]) %>% activate(tmax_day) %>% hyper_group_by(shp) %>% hyper_tibble()
  print(i)
  }
})



##---------------------------

## read all shapefiles, we
library(sf)
library(dplyr)
library(raster)
library(tabularaster)
## tidync@expt-group-by
devtools::load_all(".")

shp <-read_sf("/mnt/WineAustralia_working_files/shapes/Wine-Regions-GI-Regions/individual_shp/region_tasmania.shp")
library(ggplot2)
bfiles <- raadfiles:::bom_tmax_daily_files()
file_index <- tidync(bfiles$fullname[1]) %>% activate(tmax_day) %>% hyper_group_by(shp)
update_filename <- function(x, filename) {
  nms <- names(x)
  for (nm in nms) {
    if (!is.null(x[[nm]]$filename)) x[[nm]]$filename <- filename 
  }
  x
}

update_time <- function(x, filename) {
  hf <- tidync(filename) %>% activate(tmax_day) %>% hyper_filter()
  x$time <- hf$time
  x
}

system.time({
  out <- vector("list", nrow(bfiles))
  for (i in seq_len(nrow(bfiles))) {
    op <- options(warn = -1)
    file_index <- update_filename(file_index, bfiles$fullname[i])
    options(op)
    file_index <- update_time(file_index, bfiles$fullname[i])
    tib  <-  file_index %>% hyper_tibble()
    nc <- RNetCDF::open.nc(bfiles$fullname[i])
    tib$date <- RNetCDF::utcal.nc(RNetCDF::att.get.nc(nc, "time", "units"), tib$time, type = "c")
    RNetCDF::close.nc(nc)
    out[[i]] <- tib
    print(i)
  }
})
