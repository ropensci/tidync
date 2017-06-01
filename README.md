
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/r-gris/tidync.svg?branch=master)](https://travis-ci.org/r-gris/tidync) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-gris/tidync?branch=master&svg=true)](https://ci.appveyor.com/project/r-gris/tidync) [![Coverage Status](https://img.shields.io/codecov/c/github/r-gris/tidync/master.svg)](https://codecov.io/github/r-gris/tidync?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidync)](https://cran.r-project.org/package=tidync)

tidync
======

The goal of tidync is to ease exploring the contents of a NetCDF file and constructing efficient queries to extract arbitrary hyperslabs.

The data extracted can be used directly in array contexts, or in "long form" form "tidy" analysis and visualization contexts.

Installation
------------

You can install tidync from github with:

``` r
# install.packages("devtools")
devtools::install_github("r-gris/tidync")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
file <- system.file("extdata", "unidata", "test_hgroups.nc", package = "tidync")
library(tidync)
tidync(file) 
#> Variables: UTC_time, (mozaic_flight_2012030403540535_ascent/air_press, mozaic_flight_2012030403540535_ascent/CO, mozaic_flight_2012030403540535_ascent/O3, mozaic_flight_2012030403540535_ascent/altitude, mozaic_flight_2012030403540535_ascent/lat, mozaic_flight_2012030403540535_ascent/lon, mozaic_flight_2012030321335035_descent/CO, mozaic_flight_2012030321335035_descent/O3, mozaic_flight_2012030321335035_descent/altitude, mozaic_flight_2012030321335035_descent/UTC_time, mozaic_flight_2012030321335035_descent/lat, mozaic_flight_2012030321335035_descent/lon, mozaic_flight_2012030403540535_descent/CO, mozaic_flight_2012030403540535_descent/O3, mozaic_flight_2012030403540535_descent/altitude, mozaic_flight_2012030403540535_descent/UTC_time, mozaic_flight_2012030403540535_descent/lat, mozaic_flight_2012030403540535_descent/lon, mozaic_flight_2012030412545335_ascent/CO, mozaic_flight_2012030412545335_ascent/O3, mozaic_flight_2012030412545335_ascent/altitude, mozaic_flight_2012030412545335_ascent/UTC_time, mozaic_flight_2012030412545335_ascent/lat, mozaic_flight_2012030412545335_ascent/lon, mozaic_flight_2012030419144751_ascent/CO, mozaic_flight_2012030419144751_ascent/O3, mozaic_flight_2012030419144751_ascent/altitude, mozaic_flight_2012030419144751_ascent/UTC_time, mozaic_flight_2012030419144751_ascent/lat, mozaic_flight_2012030419144751_ascent/lon, mozaic_flight_2012030319051051_descent/CO, mozaic_flight_2012030319051051_descent/O3, mozaic_flight_2012030319051051_descent/altitude, mozaic_flight_2012030319051051_descent/UTC_time, mozaic_flight_2012030319051051_descent/lat, mozaic_flight_2012030319051051_descent/lon, mozaic_flight_2012030421382353_ascent/CO, mozaic_flight_2012030421382353_ascent/O3, mozaic_flight_2012030421382353_ascent/altitude, mozaic_flight_2012030421382353_ascent/UTC_time, mozaic_flight_2012030421382353_ascent/lat, mozaic_flight_2012030421382353_ascent/lon) 
#> Dimensions:
#> Joining, by = ".dimension_"
#> # A tibble: 2 x 5
#>   variable_name .variable_ .dimension_ dimension_name dimension_length
#>           <chr>      <dbl>       <int>          <chr>            <int>
#> 1      UTC_time          0           0         recNum               74
#> 2      UTC_time          0           0         recNum               74
```

See this article for more:

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
