
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidync

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/ropensci/tidync.svg?branch=master&env=BUILD_NAME=xenial_release&label=linux)](https://travis-ci.org/ropensci/tidync)
[![Build
Status](https://travis-ci.org/ropensci/tidync.svg?branch=master&env=BUILD_NAME=osx_release&label=osx)](https://travis-ci.org/ropensci/tidync)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ropensci/tidync?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/tidync-nvwwt)
[![Coverage
status](https://codecov.io/gh/ropensci/tidync/branch/master/graph/badge.svg)](https://codecov.io/github/ropensci/tidync?branch=master)
[![](https://badges.ropensci.org/174_status.svg)](https://github.com/ropensci/onboarding/issues/174)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidync)](https://cran.r-project.org/package=tidync)
[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/tidync)](https://cran.r-project.org/package=tidync)
<!-- badges: end -->

The goal of tidync is to ease exploring the contents of a NetCDF source
and to simplify the process of data extraction.

When extracting, data can be accessed as array/s, or in long-form as a
data frame. In contrast to other packages tidync helps reduce the volume
of code required to discover and read the contents of NetCDF, with
simple steps:

  - Connect and summarize `tidync()`.
  - (optionally) Specify source variables `activate()`.
  - (optionally) Specify array sub-setting (slicing) `hyper_filter()`.
  - Read array data in native form `hyper_array()` or long-form
    `hyper_tibble()` or bespoke form `hyper_tbl_cube()`.

NetCDF is **Network Common Data Form** a very common, and very general
way to store and work with scientific array-based data. NetCDF is
defined and provided by
[Unidata](https://www.unidata.ucar.edu/software/netcdf/). R has
(independent) support for NetCDF via the
[ncdf4](https://CRAN.R-project.org/package=ncdf4),
[rhdf5](https://bioconductor.org/packages/release/bioc/html/rhdf5.html),
[RNetCDF](https://CRAN.R-project.org/package=RNetCDF),
[rgdal](https://CRAN.R-project.org/package=rgdal),
[sf](https://CRAN.R-project.org/package=sf) and
[vapour](https://CRAN.R-project.org/package=vapour) packages.

This project uses RNetCDF for the primary access to the NetCDF library,
as well as the ncdf4 package in some cases. The wrapper provided by
[ncmeta](https://CRAN.R-project.org/package=ncmeta) over RNetCDF is used
to obtain information about data sources.

## Installation

Install tidync from CRAN.

``` r
install.packages("tidync")
```

You can install the development version from github with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/tidync", dependencies = TRUE)
```

The package packages ncdf4 and RNetCDF are required, so first make sure
you can install and use these if it doesn’t work the first time.

``` r
install.packages("ncdf4")
install.packages("RNetCDF")
```

If you have problems, please see the [INSTALL instructions for
RNetCDF](https://CRAN.R-project.org/package=RNetCDF/INSTALL), these
should work as well for ncdf4. Below I note specifics for different
operating systems, notably Ubuntu/Debian where I work the most - these
aren’t comprehensive details but might be helpful.

### Windows

On Windows, everything should be easy as ncdf4 and RNetCDF are supported
by CRAN. The RNetCDF package now includes OpenDAP/Thredds for 64-bit
Windows (not 32-bit), and so tidync will work for those sources too.

### MacOS

On MacOS, it should also be easy as there are binaries for ncdf4 and
RNetCDF available on CRAN. As far as I know, only RNetCDF will support
Thredds.

### Ubuntu/Debian

On Linux you will need at least the following installed by an
administrator, here tested on Ubuntu Xenial 16.04.

``` bash
apt update 
apt upgrade --assume-yes

## Install 3rd parties for NetCDF
apt install libnetcdf-dev libudunits2-dev

## install 3rd parties needed for devtools + openssl git2r httr
apt install libssl-dev
```

Then in R

``` r
install.packages("remotes")
remotes::install_github("ropensci/tidync")
```

At the time of writing the [travis install
configuration](https://github.com/ropensci/tidync/blob/master/.travis.yml)
was set up for “xenial”, Ubuntu 16.04 which was required for using
[NetCDF sources with
groups](https://www.unidata.ucar.edu/software/netcdf/workshops/2011/groups-types/GroupsIntro.html).

More general information about system dependencies libnetcdf-dev and
libudunits2-dev is available from [Unidata
NetCDF](https://www.unidata.ucar.edu/software/netcdf/docs/getting_and_building_netcdf.html)
and [Unidata
Udunits2](https://www.unidata.ucar.edu/software/udunits/udunits-current/doc/udunits/udunits2.html#Installation).

## Usage

This is a basic example which shows how to connect to a
file.

``` r
file <- system.file("extdata", "oceandata", "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc", package = "tidync")
library(tidync)
tidync(file) 
#> 
#> Data Source (1): S20080012008031.L3m_MO_CHL_chlor_a_9km.nc ...
#> 
#> Grids (4) <dimension family> : <associated variables> 
#> 
#> [1]   D1,D0 : chlor_a    **ACTIVE GRID** ( 9331200  values per variable)
#> [2]   D3,D2 : palette
#> [3]   D0    : lat
#> [4]   D1    : lon
#> 
#> Dimensions 4 (2 active): 
#>   
#>   dim   name  length    min   max start count   dmin  dmax unlim coord_dim 
#>   <chr> <chr>  <dbl>  <dbl> <dbl> <int> <int>  <dbl> <dbl> <lgl> <lgl>     
#> 1 D0    lat     2160  -90.0  90.0     1  2160  -90.0  90.0 FALSE TRUE      
#> 2 D1    lon     4320 -180.  180.      1  4320 -180.  180.  FALSE TRUE      
#>   
#> Inactive dimensions:
#>   
#>   dim   name          length   min   max unlim coord_dim 
#>   <chr> <chr>          <dbl> <dbl> <dbl> <lgl> <lgl>     
#> 1 D2    rgb                3     1     3 FALSE FALSE     
#> 2 D3    eightbitcolor    256     1   256 FALSE FALSE
```

There are two main ways of using tidync, interactively to explore what
is there, and for extraction. The functions `tidync` and `activate` and
`hyper_filter` allow us to hone in on the part/s of the data we want,
and functions `hyper_array`, `hyper_tibble` and `hyper_tbl_cube` give
raw-array or data frames.

Also
[http://www.matteodefelice.name/research/2018/01/14/tidyverse-and-netcdfs-a-first-exploration/](this%20blog)
post by Matteo De Felice.

### Interactive

Use `tidync()` and `hyper_filter()` to discern what variables and
dimensions are available, and to craft axis-filtering expressions by
value or by index. (Use the name of the variable on the LHS to target
it, use its name to filter by value and the special name `index` to
filter it by its
index).

``` r
filename <- system.file("extdata/argo/MD5903593_001.nc", package = "tidync")
## discover the available entities, and the active grid's dimensions and variables
tidync(filename)
#> 
#> Data Source (1): MD5903593_001.nc ...
#> 
#> Grids (16) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D9,D11,D8 : SCIENTIFIC_CALIB_DATE
#> [2]   D6,D9,D11,D8 : PARAMETER
#> [3]   D7,D9,D11,D8 : SCIENTIFIC_CALIB_EQUATION, SCIENTIFIC_CALIB_COEFFICIENT, SCIENTIFIC_CALIB_COMMENT
#> [4]   D6,D9,D8     : STATION_PARAMETERS
#> [5]   D10,D8       : PRES, PRES_QC, PRES_ADJUSTED, PRES_ADJUSTED_QC, PRES_ADJUSTED_ERROR, TEMP, TEMP_QC, TEMP_ADJUSTED, TEMP_ADJUSTED_QC, TEMP_ADJUSTED_ERROR, PSAL, PSAL_QC, PSAL_ADJUSTED, PSAL_ADJUSTED_QC, PSAL_ADJUSTED_ERROR, DOXY, DOXY_QC, DOXY_ADJUSTED, DOXY_ADJUSTED_QC, DOXY_ADJUSTED_ERROR, CHLA, CHLA_QC, CHLA_ADJUSTED, CHLA_ADJUSTED_QC, CHLA_ADJUSTED_ERROR, BBP700, BBP700_QC, BBP700_ADJUSTED, BBP700_ADJUSTED_QC, BBP700_ADJUSTED_ERROR, NITRATE, NITRATE_QC, NITRATE_ADJUSTED, NITRATE_ADJUSTED_QC, NITRATE_ADJUSTED_ERROR    **ACTIVE GRID** ( 986  values per variable)
#> [6]   D1,D8        : DATA_CENTRE
#> [7]   D2,D8        : DATA_STATE_INDICATOR, WMO_INST_TYPE
#> [8]   D3,D8        : PLATFORM_NUMBER, POSITIONING_SYSTEM
#> [9]   D5,D8        : DC_REFERENCE, PLATFORM_TYPE, FLOAT_SERIAL_NO, FIRMWARE_VERSION
#> [10]   D6,D8        : PROJECT_NAME, PI_NAME
#> [11]   D7,D8        : VERTICAL_SAMPLING_SCHEME
#> [12]   D9,D8        : PARAMETER_DATA_MODE
#> [13]   D0           : REFERENCE_DATE_TIME, DATE_CREATION, DATE_UPDATE
#> [14]   D2           : FORMAT_VERSION, HANDBOOK_VERSION
#> [15]   D5           : DATA_TYPE
#> [16]   D8           : CYCLE_NUMBER, DIRECTION, DATA_MODE, JULD, JULD_QC, JULD_LOCATION, LATITUDE, LONGITUDE, POSITION_QC, CONFIG_MISSION_NUMBER, PROFILE_PRES_QC, PROFILE_TEMP_QC, PROFILE_PSAL_QC, PROFILE_DOXY_QC, PROFILE_CHLA_QC, PROFILE_BBP700_QC, PROFILE_NITRATE_QC
#> 
#> Dimensions 14 (2 active): 
#>   
#>   dim   name     length   min   max start count  dmin  dmax unlim coord_dim 
#>   <chr> <chr>     <dbl> <dbl> <dbl> <int> <int> <dbl> <dbl> <lgl> <lgl>     
#> 1 D8    N_PROF        2     1     2     1     2     1     2 FALSE FALSE     
#> 2 D10   N_LEVELS    493     1   493     1   493     1   493 FALSE FALSE     
#>   
#> Inactive dimensions:
#>   
#>    dim   name       length   min   max unlim coord_dim 
#>    <chr> <chr>       <dbl> <dbl> <dbl> <lgl> <lgl>     
#>  1 D0    DATE_TIME      14     1    14 FALSE FALSE     
#>  2 D1    STRING2         2     1     2 FALSE FALSE     
#>  3 D2    STRING4         4     1     4 FALSE FALSE     
#>  4 D3    STRING8         8     1     8 FALSE FALSE     
#>  5 D4    STRING16       16    NA    NA FALSE FALSE     
#>  6 D5    STRING32       32     1    32 FALSE FALSE     
#>  7 D6    STRING64       64     1    64 FALSE FALSE     
#>  8 D7    STRING256     256     1   256 FALSE FALSE     
#>  9 D9    N_PARAM         7     1     7 FALSE FALSE     
#> 10 D11   N_CALIB         1     1     1 FALSE FALSE     
#> 11 D12   N_HISTORY       0    NA    NA TRUE  FALSE     
#> 12 D13   N_VALUES41     41    NA    NA FALSE FALSE

## activate a different grid
grid_identifier <- "D7,D9,D11,D8"
tidync(filename) %>% activate(grid_identifier)
#> 
#> Data Source (1): MD5903593_001.nc ...
#> 
#> Grids (16) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D9,D11,D8 : SCIENTIFIC_CALIB_DATE
#> [2]   D6,D9,D11,D8 : PARAMETER
#> [3]   D7,D9,D11,D8 : SCIENTIFIC_CALIB_EQUATION, SCIENTIFIC_CALIB_COEFFICIENT, SCIENTIFIC_CALIB_COMMENT    **ACTIVE GRID** ( 3584  values per variable)
#> [4]   D6,D9,D8     : STATION_PARAMETERS
#> [5]   D10,D8       : PRES, PRES_QC, PRES_ADJUSTED, PRES_ADJUSTED_QC, PRES_ADJUSTED_ERROR, TEMP, TEMP_QC, TEMP_ADJUSTED, TEMP_ADJUSTED_QC, TEMP_ADJUSTED_ERROR, PSAL, PSAL_QC, PSAL_ADJUSTED, PSAL_ADJUSTED_QC, PSAL_ADJUSTED_ERROR, DOXY, DOXY_QC, DOXY_ADJUSTED, DOXY_ADJUSTED_QC, DOXY_ADJUSTED_ERROR, CHLA, CHLA_QC, CHLA_ADJUSTED, CHLA_ADJUSTED_QC, CHLA_ADJUSTED_ERROR, BBP700, BBP700_QC, BBP700_ADJUSTED, BBP700_ADJUSTED_QC, BBP700_ADJUSTED_ERROR, NITRATE, NITRATE_QC, NITRATE_ADJUSTED, NITRATE_ADJUSTED_QC, NITRATE_ADJUSTED_ERROR
#> [6]   D1,D8        : DATA_CENTRE
#> [7]   D2,D8        : DATA_STATE_INDICATOR, WMO_INST_TYPE
#> [8]   D3,D8        : PLATFORM_NUMBER, POSITIONING_SYSTEM
#> [9]   D5,D8        : DC_REFERENCE, PLATFORM_TYPE, FLOAT_SERIAL_NO, FIRMWARE_VERSION
#> [10]   D6,D8        : PROJECT_NAME, PI_NAME
#> [11]   D7,D8        : VERTICAL_SAMPLING_SCHEME
#> [12]   D9,D8        : PARAMETER_DATA_MODE
#> [13]   D0           : REFERENCE_DATE_TIME, DATE_CREATION, DATE_UPDATE
#> [14]   D2           : FORMAT_VERSION, HANDBOOK_VERSION
#> [15]   D5           : DATA_TYPE
#> [16]   D8           : CYCLE_NUMBER, DIRECTION, DATA_MODE, JULD, JULD_QC, JULD_LOCATION, LATITUDE, LONGITUDE, POSITION_QC, CONFIG_MISSION_NUMBER, PROFILE_PRES_QC, PROFILE_TEMP_QC, PROFILE_PSAL_QC, PROFILE_DOXY_QC, PROFILE_CHLA_QC, PROFILE_BBP700_QC, PROFILE_NITRATE_QC
#> 
#> Dimensions 14 (4 active): 
#>   
#>   dim   name     length   min   max start count  dmin  dmax unlim coord_dim 
#>   <chr> <chr>     <dbl> <dbl> <dbl> <int> <int> <dbl> <dbl> <lgl> <lgl>     
#> 1 D7    STRING2…    256     1   256     1   256     1   256 FALSE FALSE     
#> 2 D8    N_PROF        2     1     2     1     2     1     2 FALSE FALSE     
#> 3 D9    N_PARAM       7     1     7     1     7     1     7 FALSE FALSE     
#> 4 D11   N_CALIB       1     1     1     1     1     1     1 FALSE FALSE     
#>   
#> Inactive dimensions:
#>   
#>    dim   name       length   min   max unlim coord_dim 
#>    <chr> <chr>       <dbl> <dbl> <dbl> <lgl> <lgl>     
#>  1 D0    DATE_TIME      14     1    14 FALSE FALSE     
#>  2 D1    STRING2         2     1     2 FALSE FALSE     
#>  3 D2    STRING4         4     1     4 FALSE FALSE     
#>  4 D3    STRING8         8     1     8 FALSE FALSE     
#>  5 D4    STRING16       16    NA    NA FALSE FALSE     
#>  6 D5    STRING32       32     1    32 FALSE FALSE     
#>  7 D6    STRING64       64     1    64 FALSE FALSE     
#>  8 D10   N_LEVELS      493     1   493 FALSE FALSE     
#>  9 D12   N_HISTORY       0    NA    NA TRUE  FALSE     
#> 10 D13   N_VALUES41     41    NA    NA FALSE FALSE

## pass named expressions to subset dimension by value or index (step)
(subs <- tidync(filename) %>% hyper_filter(N_PROF = N_PROF > 1, STRING256 = index > 10))
#> Warning in hyper_filter.tidync(., N_PROF = N_PROF > 1, STRING256 = index
#> > : 'STRING256' not found in active grid, ignoring
#> 
#> Data Source (1): MD5903593_001.nc ...
#> 
#> Grids (16) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D9,D11,D8 : SCIENTIFIC_CALIB_DATE
#> [2]   D6,D9,D11,D8 : PARAMETER
#> [3]   D7,D9,D11,D8 : SCIENTIFIC_CALIB_EQUATION, SCIENTIFIC_CALIB_COEFFICIENT, SCIENTIFIC_CALIB_COMMENT
#> [4]   D6,D9,D8     : STATION_PARAMETERS
#> [5]   D10,D8       : PRES, PRES_QC, PRES_ADJUSTED, PRES_ADJUSTED_QC, PRES_ADJUSTED_ERROR, TEMP, TEMP_QC, TEMP_ADJUSTED, TEMP_ADJUSTED_QC, TEMP_ADJUSTED_ERROR, PSAL, PSAL_QC, PSAL_ADJUSTED, PSAL_ADJUSTED_QC, PSAL_ADJUSTED_ERROR, DOXY, DOXY_QC, DOXY_ADJUSTED, DOXY_ADJUSTED_QC, DOXY_ADJUSTED_ERROR, CHLA, CHLA_QC, CHLA_ADJUSTED, CHLA_ADJUSTED_QC, CHLA_ADJUSTED_ERROR, BBP700, BBP700_QC, BBP700_ADJUSTED, BBP700_ADJUSTED_QC, BBP700_ADJUSTED_ERROR, NITRATE, NITRATE_QC, NITRATE_ADJUSTED, NITRATE_ADJUSTED_QC, NITRATE_ADJUSTED_ERROR    **ACTIVE GRID** ( 986  values per variable)
#> [6]   D1,D8        : DATA_CENTRE
#> [7]   D2,D8        : DATA_STATE_INDICATOR, WMO_INST_TYPE
#> [8]   D3,D8        : PLATFORM_NUMBER, POSITIONING_SYSTEM
#> [9]   D5,D8        : DC_REFERENCE, PLATFORM_TYPE, FLOAT_SERIAL_NO, FIRMWARE_VERSION
#> [10]   D6,D8        : PROJECT_NAME, PI_NAME
#> [11]   D7,D8        : VERTICAL_SAMPLING_SCHEME
#> [12]   D9,D8        : PARAMETER_DATA_MODE
#> [13]   D0           : REFERENCE_DATE_TIME, DATE_CREATION, DATE_UPDATE
#> [14]   D2           : FORMAT_VERSION, HANDBOOK_VERSION
#> [15]   D5           : DATA_TYPE
#> [16]   D8           : CYCLE_NUMBER, DIRECTION, DATA_MODE, JULD, JULD_QC, JULD_LOCATION, LATITUDE, LONGITUDE, POSITION_QC, CONFIG_MISSION_NUMBER, PROFILE_PRES_QC, PROFILE_TEMP_QC, PROFILE_PSAL_QC, PROFILE_DOXY_QC, PROFILE_CHLA_QC, PROFILE_BBP700_QC, PROFILE_NITRATE_QC
#> 
#> Dimensions 14 (2 active): 
#>   
#>   dim   name     length   min   max start count  dmin  dmax unlim coord_dim 
#>   <chr> <chr>     <dbl> <dbl> <dbl> <int> <int> <dbl> <dbl> <lgl> <lgl>     
#> 1 D8    N_PROF        2     1     2     2     1     2     2 FALSE FALSE     
#> 2 D10   N_LEVELS    493     1   493     1   493     1   493 FALSE FALSE     
#>   
#> Inactive dimensions:
#>   
#>    dim   name       length   min   max unlim coord_dim 
#>    <chr> <chr>       <dbl> <dbl> <dbl> <lgl> <lgl>     
#>  1 D0    DATE_TIME      14     1    14 FALSE FALSE     
#>  2 D1    STRING2         2     1     2 FALSE FALSE     
#>  3 D2    STRING4         4     1     4 FALSE FALSE     
#>  4 D3    STRING8         8     1     8 FALSE FALSE     
#>  5 D4    STRING16       16    NA    NA FALSE FALSE     
#>  6 D5    STRING32       32     1    32 FALSE FALSE     
#>  7 D6    STRING64       64     1    64 FALSE FALSE     
#>  8 D7    STRING256     256     1   256 FALSE FALSE     
#>  9 D9    N_PARAM         7     1     7 FALSE FALSE     
#> 10 D11   N_CALIB         1     1     1 FALSE FALSE     
#> 11 D12   N_HISTORY       0    NA    NA TRUE  FALSE     
#> 12 D13   N_VALUES41     41    NA    NA FALSE FALSE

## with the saved filtering from above, choose data frame or tbl_cube output
## optionally with only selected variables
subs %>% hyper_tibble()
#> # A tibble: 493 x 37
#>     PRES PRES_QC PRES_ADJUSTED PRES_ADJUSTED_QC PRES_ADJUSTED_E…  TEMP
#>    <dbl> <chr>           <dbl> <chr>                       <dbl> <dbl>
#>  1  7.70 1                7.79 1                            2.40  13.2
#>  2 11.8  1               11.9  1                            2.40  13.2
#>  3 16.3  1               16.4  1                            2.40  13.2
#>  4 21.6  1               21.7  1                            2.40  13.2
#>  5 26.7  1               26.8  1                            2.40  13.2
#>  6 31.7  1               31.8  1                            2.40  13.2
#>  7 36.6  1               36.7  1                            2.40  13.2
#>  8 41.4  1               41.5  1                            2.40  13.2
#>  9 46.5  1               46.6  1                            2.40  13.2
#> 10 51.8  1               51.9  1                            2.40  13.2
#> # … with 483 more rows, and 31 more variables: TEMP_QC <chr>,
#> #   TEMP_ADJUSTED <dbl>, TEMP_ADJUSTED_QC <chr>,
#> #   TEMP_ADJUSTED_ERROR <dbl>, PSAL <dbl>, PSAL_QC <chr>,
#> #   PSAL_ADJUSTED <dbl>, PSAL_ADJUSTED_QC <chr>,
#> #   PSAL_ADJUSTED_ERROR <dbl>, DOXY <dbl>, DOXY_QC <chr>,
#> #   DOXY_ADJUSTED <dbl>, DOXY_ADJUSTED_QC <chr>,
#> #   DOXY_ADJUSTED_ERROR <dbl>, CHLA <dbl>, CHLA_QC <chr>,
#> #   CHLA_ADJUSTED <dbl>, CHLA_ADJUSTED_QC <chr>,
#> #   CHLA_ADJUSTED_ERROR <dbl>, BBP700 <dbl>, BBP700_QC <chr>,
#> #   BBP700_ADJUSTED <dbl>, BBP700_ADJUSTED_QC <chr>,
#> #   BBP700_ADJUSTED_ERROR <dbl>, NITRATE <dbl>, NITRATE_QC <chr>,
#> #   NITRATE_ADJUSTED <dbl>, NITRATE_ADJUSTED_QC <chr>,
#> #   NITRATE_ADJUSTED_ERROR <dbl>, N_PROF <int>, N_LEVELS <int>
subs %>% hyper_tbl_cube(select_var = c("PRES", "PRES_QC", "PSAL_ADJUSTED"))
#> Source: local array [493 x 2]
#> D: N_LEVELS [int, 493]
#> D: N_PROF [int, 1]
#> M: PRES [dbl]
#> M: PRES_QC [chr[,1]]
#> M: PSAL_ADJUSTED [dbl]
```

A grid is a “virtual table” in the sense of a database source. It’s
possible to activate a grid via a variable within it, so all variables
are available by default. Grids have identifiers based on which
dimensions they are defined with, so use i.e. “D1,D0” and can otherwise
be activated by their count identifier (starting at 1). The “D0” is an
identifier, it matches the internal 0-based indexing and identity used
by NetCDF itself.

### Extractive

Use what we learned interactively to extract the data, either in data
frame or raw-array (hyper slice) form.

``` r
## we'll see a column for the variable activated, and whatever other 
## variables the grid has
tidync(filename) %>% activate("JULD") %>% 
  hyper_filter(N_PROF = N_PROF == 1) %>% 
  hyper_tibble()
#> # A tibble: 98 x 5
#>    SCIENTIFIC_CALIB_DATE DATE_TIME N_PROF N_PARAM N_CALIB
#>    <chr>                     <int>  <int>   <int>   <int>
#>  1 2                             1      1       1       1
#>  2 0                             2      1       1       1
#>  3 1                             3      1       1       1
#>  4 7                             4      1       1       1
#>  5 0                             5      1       1       1
#>  6 4                             6      1       1       1
#>  7 1                             7      1       1       1
#>  8 0                             8      1       1       1
#>  9 1                             9      1       1       1
#> 10 4                            10      1       1       1
#> # … with 88 more rows


## native array form, we'll see a (list of) R arrays with a dimension for 
## each seen by tidync(filename) %>% activate("JULD")
tidync(filename) %>% activate("JULD") %>% 
  hyper_filter(N_PROF = N_PROF == 1) %>% 
  hyper_array()
#> Tidync Data Arrays
#> Variables (1): 'SCIENTIFIC_CALIB_DATE'
#> Dimension (4): 14, 7, 1, 1
#> Source: /perm_storage/home/mdsumner/R/x86_64-pc-linux-gnu-library/3.6/tidync/extdata/argo/MD5903593_001.nc
```

It’s important to not actual request the data extraction until the
expressions above would result in an efficient size (don’t try a data
frame version of a 20Gb ROMs variable …). Use the interactive modes to
determine the likely size of the output you will receive.

Functions seamlessly build the actual index values required by the
NetCDF library. This can be used to debug the process or to define your
own tools for the extraction. Currently each `hyper_*` function can take
the filtering expressions, but it’s not obvious if this is a good idea
or not.

See the vignettes for more:

``` r
browseVignettes(package = "tidync")
```

## Limitations

Please get in touch if you have specific workflows that `tidync` is not
providing. There’s a lot of room for improvement\!

  - we can’t do “grouped filters”" (i.e. polygon-overlay extraction),
    but it’s in the works
  - compound types are not supported, though see the “rhdf5” branch on
    Github
  - NetCDF groups are not exposed (groups are like a “files within a
    file”, analogous to a file system directory)

I’m interested in lighter and rawer access to the NetCDF library, I’ve
explored that here and it may or may not be a good idea:

<https://github.com/hypertidy/ncapi>

## Terminology

  - **slab**, **hyperslab** - array variable that may be read from a
    NetCDF
  - **shape**, **grid** - set of dimensions that define variables in
    NetCDF
  - **activation** - choice of a given grid to apply subsetting and read
    operations to

-----

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its
terms.

[![ropensci\_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
