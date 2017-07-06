u <- "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day"

library(tidync)
(tnc <- tidync())
(ftnc <- hyper_filter())

## now build an an explicit extraction
ht <- tidync(u) %>% hyper_filter(longitude = longitude > 142 & longitude < 150, 
                                 latitude = between(latitude, -44, -40), 
                                 time = index %% 400 == 0) %>% 
  hyper_tibble()




library(ggplot2)
ggplot(ht, aes(longitude, latitude, fill = sqrt(x_wind^2 + y_wind^2))) + 
  geom_raster() + facet_wrap(~time)

ht[c("x", "y")] <- tibble::as_tibble(rgdal::project(as.matrix(dplyr::select(ht, longitude, latitude)), "+proj=laea +lon_0=160 +lat_0=-10 +datum=WGS84"))
const <- 7200

ggplot(ht %>% dplyr::filter(!is.na(x_wind)) %>% sample_frac(0.1), aes(x, y, xend = x + x_wind * const, yend = y + y_wind * const)) + 
  geom_segment() + facet_wrap(~time)





