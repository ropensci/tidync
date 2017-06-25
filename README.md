
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/hypertidy/tidync.svg?branch=master)](https://travis-ci.org/hypertidy/tidync) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/tidync?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/tidync)

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidync)](https://cran.r-project.org/package=tidync)

tidync
======

**NOTE:** this package is development and subject to change. See Limitations below. Active design discussed here: <https://github.com/hypertidy/tidync/issues/33>

The goal of tidync is to ease exploring the contents of a NetCDF file and constructing efficient queries to extract arbitrary hyperslabs.

The data extracted can be used directly in array contexts, or in "long form" form "tidy" analysis and visualization contexts.

There are two main ways of using tidync.

These examples are for illustration only, see the vignettes for more details, and please try on your own files!

Interactive
-----------

Use `tidync()` and `hyper_filter()` to discern what variables and dimensions are available, and to craft axis-filtering expressions by value or by index. (Use the name of the variable on the LHS to target it, use its name to filter by value and the special name `index` to filter it by it's 'step' index).

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

Note, an earlier version of this package worked by activating "variables by name". This still works but effectively activates the grid that this variable exists within, so all variables in the space are available by default. Grids have identifiers based on which dimensions they use i.e. "D1,D0" and can otherwise be activated by their count identifier (starting at 1).

Extractive
----------

Use what we learned interactively to extract the data, either in data frame or raw-array (hyper slice) form. It's important to not actual request the data extraction until the expressions above would result in an efficient size (don't try a data frame version of a 20Gb ROMs variable ...).

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

There is another function `hyper_index` that build the actual index values required by the NetCDF library. This can be used to debug the process or to define your own tools for the extraction. Currently each `hyper_*` function can take the filtering expressions, but it's not obvious if this is a good idea or not.

Development
===========

Wishlist. Submit your own to the [Issues tab](https://github.com/hypertidy/tidync)

-   install useful example files, and steps to download good files
-   figure out whether "activate" is the right function, how to use that name (sorry Thomas)
-   wrappers for returning raster brick
-   helpers for dimension values as lists of coordinates for the slice form
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
#>   dimension    id          name length unlim 
#>       <chr> <dbl>         <chr>  <dbl> <lgl> 
#> 1        D0     0           lat   2160 FALSE 
#> 2        D1     1           lon   4320 FALSE 
#> 3        D2     2           rgb      3 FALSE 
#> 4        D3     3 eightbitcolor    256 FALSE
```

See this article for more: <https://hypertidy.github.io/tidync/articles/static-vignettes/tidync-examples.html>

Stay tuned.

Limitations
-----------

-   we can't filter on multidimensional values (yet)
-   compound types are not supported
-   groups are not exposed as entities
-   dims without variables are not handled currently, they should at least work in "index" form but don't yet <https://github.com/hypertidy/tidync/issues/30>
-   testing has so far been minimal, and things will change

Terminology
-----------

I think of "slab" as a generalized "array" (in the R sense) that we can request from a NetCDF. We must provide the NetCDF API with a "slab index", i.e. a start and a count vector - and that's the only way to query them.

In R terms a 3D array would be indexed like

arr\[1:10, 2:12, 3:5\]

and that would be analogous to

ncvar\_get(con, start = c(1, 2, 3), count = c(10, 11, 3))

If we only wanted a "sparse trace" through the array in R we can do

arr\[cbind(c(2, 4), c(5, 6), c(3, 4)\]

which would pull out 2-values from 2 arbitrary positions. The API doesn't allow that (at least not in an efficient way that I can understand).

We either have to get the whole "slab" that encompases those 2 cells, or request a degenerate 1-cell slab for each:

ncvar\_get(con, start = c(2, 5, 3), count = c(1, 1, 1)) ncvar\_get(con, start = c(4, 6, 4), count = c(1, 1, 1))

I've used the term "hyperslab" and "slab" since I realized this basic limitation during my PhD. Unidata use the term but it's not in the API afaik:

<http://www.unidata.ucar.edu/software/netcdf/docs/netcdf_data_set_components.html>

Another term like this is "shape" which is a particular set of dimensions used by 1 or more variables in a file. tidync aims to allow "activation" of a given shape, so that any subsequent extraction gets all the variables that live in that space/shape. (It's a database table interpretation of a set of variables).

In R we can determine those indexes really easily for any given query, tracks over time through XYZ, polygons, boxes and so on - but we are ultimately limited by the API to these "slab" requests, so you see a lot of disparate approaches across packages to optimizing this.

We have decided to use the term "grid" rather than "shape", and so a grid is a specific ordered set of dimensions. An "axis" is a particular instance of a dimension within a variable. At times we need to know what grids we have, what variables use those grids, and what axes belong to a variable and in what order. Currently all the facility for determining this information is in package ncmeta.

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
