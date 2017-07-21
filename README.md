
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hypertidy/tidync.svg?branch=master)](https://travis-ci.org/hypertidy/tidync) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/tidync?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/tidync)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidync)](https://cran.r-project.org/package=tidync)

tidync
======

The goal of tidync is to ease exploring the contents of a NetCDF file and constructing efficient queries to extract arbitrary hyperslabs.

The data extracted can be used directly as an array, or in "long form" form as a data frame for "tidy" analysis and visualization contexts.

These examples are for illustration, see the vignettes for more details, and please try on your own sources!

There are two main ways of using tidync.

Interactive
-----------

Use `tidync()` and `hyper_filter()` to discern what variables and dimensions are available, and to craft axis-filtering expressions by value or by index. (Use the name of the variable on the LHS to target it, use its name to filter by value and the special name `index` to filter it by its 'step' index).

``` r
## discover the available entities, and the active grid's dimensions and variables
tidync(filename)

## activate a different grid
tidync(filename) %>% activate(grid_identifier)

## get a dimension-focus on the space occupied within a grid
tidync(filename) %>% hyper_filter()

## pass named expressions to subset dimension by value or index (step)
tidync(filename) %>% hyper_filter(lat = lat < -30, time = time == 20)
```

A grid is a "virtual table" in the sense of a database source. It's possible to activate a grid via a variable within it, so all variables are available by default. Grids have identifiers based on which dimensions they are defined with, so use i.e. "D1,D0" and can otherwise be activated by their count identifier (starting at 1). The "D0" is an identifier, it matches the internal 0-based indexing and identity used by NetCDF itself.

Extractive
----------

Use what we learned interactively to extract the data, either in data frame or raw-array (hyper slice) form.

``` r
## we'll see a column for sst, lat, time, and whatever other dimensions sst has
## and whatever other variable's the grid has
tidync(filename) %>% activate("sst"") %>% 
  hyper_filter(lat = lat < -30, time = time == 20) %>% 
  hyper_tibble()


## raw array form, we'll see a (list of) R arrays with a dimension for each seen by tidync(filename) %>% activate("sst"")
tidync(filename) %>% activate("sst"") %>% 
  hyper_filter(lat = lat < -30, time = time == 20) %>% 
  hyper_slice()
```

It's important to not actual request the data extraction until the expressions above would result in an efficient size (don't try a data frame version of a 20Gb ROMs variable ...). Use the interactive modes to determine the likely size of the output you will receive.

There is another function `hyper_index` that build the actual index values required by the NetCDF library. This can be used to debug the process or to define your own tools for the extraction. Currently each `hyper_*` function can take the filtering expressions, but it's not obvious if this is a good idea or not.

Development
===========

Wishlist items here. Submit your own to the [Issues tab](https://github.com/hypertidy/tidync)

-   wrappers for returning various formats, like raster brick, simple features, rgl quad mesh forms, etc.
-   delayed extraction to show the tibble you would get after collect()
-   consider better function names like `hyper_df`, `hyper_dbl` and `hyper_int` (differentiate scaled and unscaled?)

Support for tbl\_cube is in bare-bones form. See here for an example: <http://rpubs.com/cyclemumner/281801>

    f <- "eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/1981/AVHRR/avhrr-only-v2.19810901.nc"
    tidync(f) %>% hyper_tbl_cube(lat = lat > -30)
    Source: local array [691,200 x 4]
    D: lon [dbl, 1440]
    D: lat [dbl, 480]
    D: zlev [dbl, 1]
    D: time [dbl, 1]
    M: sst [dbl]

    tidync(f) %>% activate("anom"") %>% hyper_tbl_cube(lat = lat > -30)
    Source: local array [691,200 x 4]
    D: lon [dbl, 1440]
    D: lat [dbl, 480]
    D: zlev [dbl, 1]
    D: time [dbl, 1]
    M: anom [dbl]

Installation
------------

You can install tidync from github with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/tidync")
```

Example
-------

This is a basic example which shows you how to connect to a file.

``` r
file <- system.file("extdata", "oceandata", "S20092742009304.L3m_MO_CHL_chlor_a_9km.nc", package = "tidync")
library(tidync)
#> Warning: Installed Rcpp (0.12.12) different from Rcpp used to build dplyr (0.12.11).
#> Please reinstall dplyr to avoid random crashes or undefined behavior.
tidync(file) 
#> 
#> Data Source (1): S20092742009304.L3m_MO_CHL_chlor_a_9km.nc ...
#> 
#> Grids (4) <dimension family> : <associated variables> 
#> 
#> [1]   D1,D0 : chlor_a    **ACTIVE GRID** ( 9331200  values per variable)
#> [2]   D3,D2 : palette
#> [3]   D0    : lat
#> [4]   D1    : lon
#> 
#> Dimensions (4): 
#>   
#>   dimension    id          name length unlim coord_dim 
#>       <chr> <dbl>         <chr>  <dbl> <lgl>     <lgl> 
#> 1        D0     0           lat   2160 FALSE      TRUE 
#> 2        D1     1           lon   4320 FALSE      TRUE 
#> 3        D2     2           rgb      3 FALSE     FALSE 
#> 4        D3     3 eightbitcolor    256 FALSE     FALSE
```

See this article for more: <https://hypertidy.github.io/tidync/articles/static-vignettes/tidync-examples.html>

Limitations
-----------

-   we can't do grouped filters (i.e. polygon-overlay extraction), but it's in the works
-   compound types are not supported, though see the "rhdf5" branch on Github
-   groups are not exposed

Terminology
-----------

I think of "slab" as a generalized "array" (in the R sense) that we can request from a NetCDF. We must provide the NetCDF API with a "slab index", i.e. a start and a count vector - and that's the only way to query them.

In R terms a 3D array would be indexed like

``` r
arr[1:10, 2:12, 3:5]
```

and that would be analogous to

``` r
ncvar_get(con, start = c(1, 2, 3), count = c(10, 11, 3))
```

If we only wanted a "sparse trace" through the array in R we can do

``` r
arr[cbind(c(2, 4), c(5, 6), c(3, 4)]
```

which would pull out 2-values from 2 arbitrary positions. The API doesn't allow that (at least not in an efficient way that I can understand).

We either have to get the whole "slab" that encompases those 2 cells, or request a degenerate 1-cell slab for each:

``` r
ncvar_get(con, start = c(2, 5, 3), count = c(1, 1, 1))
ncvar_get(con, start = c(4, 6, 4), count = c(1, 1, 1))
```

I've used the term "hyperslab" and "slab" since I realized this basic limitation during my PhD. Unidata use the term but it's not in the API afaik:

<http://www.unidata.ucar.edu/software/netcdf/docs/netcdf_data_set_components.html>

Another term like this is "shape" which is a particular set of dimensions used by 1 or more variables in a file. tidync aims to allow "activation" of a given shape, so that any subsequent extraction gets all the variables that live in that space/shape. (It's a database table interpretation of a set of variables).

In R we can determine those indexes really easily for any given query, tracks over time through XYZ, polygons, boxes and so on - but we are ultimately limited by the API to these "slab" requests, so you see a lot of disparate approaches across packages to optimizing this.

We have decided to use the term "grid" rather than "shape", and so a grid is a specific ordered set of dimensions. An "axis" is a particular instance of a dimension within a variable. At times we need to know what grids we have, what variables use those grids, and what axes belong to a variable and in what order. Currently all the facility for determining this information is in package ncmeta.

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
