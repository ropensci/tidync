library(tidync)
f <- tail(raadtools::sstfiles()$fullname, 1)
library(dplyr)
tab <- tidync(f) %>% hyper_tibble(lon = lon > 50 & lon < 100, lat = lat > -75 & lat < -45) %>% 
  dplyr::select(anom, err, ice, sst, lon, lat)

library(tidyr)

tablong <- tab %>% gather(variable, measure, -lon, -lat) %>% 
  dplyr::filter(!is.na(measure))
library(ggplot2)
ggplot(tablong, aes(x = lon, y = lat, fill = measure)) + 
  geom_raster() + facet_wrap(~variable)
