library(tidync)

erddap <- function(datasetid,
                        url = "https://upwell.pfeg.noaa.gov/erddap/") {
  sprintf("%sgriddap/%s", url, datasetid)
}
tidy_erddap <- function(datasetid, xlim = NULL, ylim = NULL, tlim = NULL) {
  src <- erddap(datasetid)
  tnc <- tidync(src)
  if (!is.null(xlim)) {
    xlim <- range(xlim)
    tnc$transforms$longitude <-
      dplyr::mutate(tnc$transforms$longitude, selected = dplyr::between(.data$longitude, xlim[1], xlim[2]))
  }

  if (!is.null(ylim)) {
    ylim <- range(ylim)
    tnc$transforms$latitude <- dplyr::mutate(tnc$transforms$latitude, selected = dplyr::between(.data$latitude, ylim[1], ylim[2]))
  }
  if (!is.null(tlim)) {
    tlim <- range(as.POSIXct(tlim))
    nc <- ncmeta::nc_meta(tnc$source$source)
    ht <- hyper_transforms(tnc)
    time0 <- RNetCDF::utcal.nc((nc$attribute %>% dplyr::filter(variable == "time", name == "units") %>% dplyr::pull(value))[[1]], ht$time$time)
    time_real <- ISOdatetime(time0[,1], time0[,2], time0[,3], time0[,4], time0[,5], time0[,6], tz = "UTC")
    tnc$transforms$time <- dplyr::mutate(tnc$transforms$time, selected = dplyr::between(time_real, tlim[1], tlim[2]))
    print("setting time range")
  }
  tidync:::update_slices(tnc)  ## it should NOT work this way
}

nc <- tidy_erddap("jplMURSST41", tlim =  c("2019-10-10", "2019-10-13"))
out <- nc %>% hyper_filter(longitude = between(longitude, 122, 154), latitude = between(latitude, 24, 46))

# murSST <- griddap(info("jplMURSST41"),
#                   latitude  = c(24., 46.),
#                   longitude = c(122., 154.),
#                   time      = c("2019-10-10", "2019-10-13"),
#                   fields    = "analysed_sst")

