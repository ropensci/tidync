
<!-- README.md is generated from README.Rmd. Please edit that file -->
tidync
======

[![](https://badges.ropensci.org/174_status.svg)](https://github.com/ropensci/onboarding/issues/174) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/tidync)](https://cran.r-project.org/package=tidync)

The goal of tidync is to ease exploring the contents of a NetCDF source and constructing efficient queries to extract arbitrary hyperslabs. The data extracted can be used directly as an array, or in "long form" form as a data frame for "tidy" analysis and visualization contexts.

NetCDF is **Network Common Data Form** a very common, and very general way to store and work with scientific array-based data. NetCDF is defined and provided by [Unidata](https://www.unidata.ucar.edu/software/netcdf/). R has (independent) support for NetCDF via the [RNetCDF](https://CRAN.R-project.org/package=RNetCDF), [ncdf4](https://CRAN.R-project.org/package=ncdf4), [rhdf5](https://bioconductor.org/packages/release/bioc/html/rhdf5.html) and [rgdal](https://CRAN.R-project.org/package=rgdal) packages.

This project uses RNetCDF for the primary access to the NetCDF library.

Installation
------------

[![Travis-CI Build Status](https://travis-ci.org/hypertidy/tidync.svg?branch=master)](https://travis-ci.org/hypertidy/tidync) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/hypertidy/tidync?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/tidync) [![Coverage status](https://codecov.io/gh/hypertidy/tidync/branch/master/graph/badge.svg)](https://codecov.io/github/hypertidy/tidync?branch=master)

You can install tidync from github with:

``` r
# install.packages("devtools")
devtools::install_github("hypertidy/tidync", dependencies = TRUE)
```

Usage
-----

This is a basic example which shows how to connect to a file.

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
#>   dimension    id name           length unlim coord_dim active start count 
#>   <chr>     <dbl> <chr>           <dbl> <lgl> <lgl>     <lgl>  <int> <int> 
#> 1 D0         0    lat           2160    F     T         T          1  2160 
#> 2 D1         1.00 lon           4320    F     T         T          1  4320 
#> 3 D2         2.00 rgb              3.00 F     F         F          1     3 
#> 4 D3         3.00 eightbitcolor  256    F     F         F          1   256
```

There are two main ways of using tidync, interactively to explore what is there, and for extraction. The functions `tidync` and `activate` and `hyper_filter` allow us to hone in on the part/s of the data we want, and functions `hyper_slice` and `hyper_tibble` give raw-array and data frames with-full-coordinates forms respectively.

### Interactive

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

### Extractive

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

Functions seamlessly build the actual index values required by the NetCDF library. This can be used to debug the process or to define your own tools for the extraction. Currently each `hyper_*` function can take the filtering expressions, but it's not obvious if this is a good idea or not.

See this article for more: <https://hypertidy.github.io/tidync/articles/static-vignettes/tidync-examples.html>

Limitations
-----------

Plesase get in touch if you have specfic workflows that `tidync` is not providing. There's a lot of room for improvement!

-   we can't do "grouped filters"" (i.e. polygon-overlay extraction), but it's in the works
-   compound types are not supported, though see the "rhdf5" branch on Github
-   NetCDF groups are not exposed (groups are like a "files within a file", analogous to a file system directory)

I'm interested in lighter and rawer access to the NetCDF library, I've explored that here and it may or may not be a good idea:

<https://github.com/hypertidy/ncapi>

Terminology
-----------

Here I use the term "slab" as a generalized "array" (in the R sense) that may be read from a NetCDF. We must provide the NetCDF API with a "slab index", i.e. both a start and a count vector each the same length (the same as the number of dimensions as the array variable), that is the only way to read them.

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

I've used the term "hyperslab" and "slab" since I realized this basic limitation during earlier work. Unidata use the term but it's not in the API afaik:

<http://www.unidata.ucar.edu/software/netcdf/docs/netcdf_data_set_components.html>

Another term like this is "shape" which is a particular set of dimensions used by 1 or more variables in a file. tidync aims to allow "activation" of a given shape, so that any subsequent extraction gets all the variables that live in that space/shape. (It's a database table interpretation of a set of variables).

In R we can determine those indexes really easily for any given query, tracks over time through XYZ, polygons, boxes and so on - but we are ultimately limited by the API to these "slab" requests, so you see a lot of disparate approaches across packages to optimizing this.

For `tidync` we decided to use the term "grid" rather than "shape", and so a grid is the space defined by specific ordered set of dimensions. An "axis" is a particular instance of a dimension within a variable. At times we need to know what grids we have, what variables use those grids, and what axes belong to a variable and in what order. Currently all the facility for determining this information is in package `ncmeta`.

The `ncmeta` package uses a mix of background packages `ncdf4`, `RNetCDF` because each has facilities and limitations and both are needed. For closer to full support we can also use `rhdf5` or `rgdal` which have indpendent wrappers around the core NetCDF library. Groups and compound types are particular challenges, and tidync and ncmeta don't yet exploit all of the currently available facilities. It would still be good to have a single authoratative wrapper, but the details of that aren't widely discussed.

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
