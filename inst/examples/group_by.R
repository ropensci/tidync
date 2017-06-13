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
library(ggplot2)


## this point-in-polygon test needs review here, sp is uncharacteristically slow
system.time(d <- tidync(bom) %>% activate(tmax_day) %>% hyper_group_by(shp) %>% hyper_tibble())
d
ggplot(d, aes(longitude, latitude, fill = tmax_day)) + geom_raster() +  facet_wrap(~time) + coord_equal()


shp <-read_sf("/mnt/WineAustralia_working_files/shapes/Wine-Regions-GI-Regions/individual_shp/region_adelaide_hills.shp")
system.time(d <- tidync(bom) %>% activate(tmax_day) %>% hyper_group_by(shp) %>% hyper_tibble())
d
ggplot(d, aes(longitude, latitude, fill = tmax_day)) + geom_raster() +  facet_wrap(~time)
