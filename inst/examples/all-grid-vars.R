library(tidync)
library(dplyr)
library(tidyr)
library(ggplot2)

f <- tail(raadtools::sstfiles()$fullname, 1)

tab <- tidync(f) %>% hyper_tibble(lon = lon > 50 & lon < 100, lat = lat > -75 & lat < -45) %>% 
  dplyr::select(anom, err, ice, sst, lon, lat)


tablong <- tab %>% gather(variable, measure, -lon, -lat) %>% 
  dplyr::filter(!is.na(measure))
ggplot(tablong, aes(x = lon, y = lat, fill = measure)) + 
  geom_raster() + facet_wrap(~variable)
