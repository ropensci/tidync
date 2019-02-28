---
title: "NetCDF examples"
author: "Michael D. Sumner"
date: "2019-02-28"
output:
  rmarkdown::html_vignette:
    fig_width: 10
    fig_height: 10
vignette: >
  %\VignetteIndexEntry{Tidy NetCDF examples}
  %\VignetteEngine{knitr::knitr}
  %\VignetteEncoding{UTF-8}
---



The goal of tidync is to ease exploring the contents of a NetCDF source and constructing efficient 
queries to extract arbitrary hyperslabs. The data extracted can be used directly as an array, or in 
"long form" form as a data frame for "tidy" analysis and visualization contexts. 

NetCDF is **Network Common Data Form**, a [data model][not-a-format] and an [API](https://en.wikipedia.org/wiki/Application_programming_interface).

This is a very common, and *very general* way to store and work with 
scientific array-based data. NetCDF is defined and provided by [Unidata](https://www.unidata.ucar.edu/software/netcdf/).

Here we introduce traditional concepts of NetCDF, and show examples with built-in files and online sources to demonstrate tidync functionality. 

# NetCDF

NetCDF is a very widely used file format for storing array-based data as *variables*. The **space** occupied by a **variable** is defined by its **dimensions** and their metadata. Dimensions are by definition *one-dimensional* (i.e. an atomic vector, in R) consisting of one or more elements, a rectilinear virtual array with coordinate metadata on its units, type and interpretation. The **space** of a variable is defined as one or more of the dimensions in the file, but a variable won't necessarily use all the available dimensions and no dimensions are mandatory or particularly special. 

Some conventions exist to define usage and minimal standards for metadata for particular file schemas, but these are many and varied, and not always adhered to. 

The R community is not particularly strong with use of NetCDF, though it
is common and widely used it pales compared to use in general climate science work, and there the most used tool is the [CDO Climate Data Operators](https://code.mpimet.mpg.de/projects/cdo).  In R the most common
tools used are ncdf4 and raster (which uses ncdf4). 

Both the RNetCDF and ncdf4 packages provide a traditional summary format, familiar to many NetCDF users as the output of the command line program [`ncdump`](https://www.unidata.ucar.edu/software/netcdf/netcdf-4/newdocs/netcdf/NetCDF-Utilities.html#NetCDF-Utilities). 



```r
f <- system.file("extdata", "ifremer", "20171002.nc", package = "tidync")
library(RNetCDF)
print.nc(open.nc(f))
#> dimensions:
#>         ni = 632 ;
#>         nj = 664 ;
#>         time = 1 ;
#> variables:
#>         int time(time) ;
#>                 time:long_name = "time" ;
#>                 time:units = "hours since 1900-1-1 0:0:0" ;
#>         byte concentration(ni, nj, time) ;
#>                 concentration:long_name = "sea-ice concentration" ;
#>                 concentration:units = "percent" ;
#>                 concentration:scale_factor = 1 ;
#>                 concentration:add_offset = 0 ;
#>                 concentration:missing_value = -128 ;
#>                 concentration:_FillValue = -128 ;
#>         byte quality_flag(ni, nj, time) ;
#>                 quality_flag:long_name = "quality_flag" ;
#>                 quality_flag:units = "n/a" ;
#>                 quality_flag:scale_factor = 1 ;
#>                 quality_flag:add_offset = 0 ;
#>                 quality_flag:missing_value = -128 ;
#>                 quality_flag:_FillValue = -128 ;
#> 
#> // global attributes:
#>                 :CONVENTIONS = "COARDS" ;
#>                 :long_name = "Sea-ice concentration as observed by SSM/I" ;
#>                 :short_name = "PSI-F18-Concentration" ;
#>                 :producer_agency = "IFREMER" ;
#>                 :producer_institution = "CERSAT" ;
#>                 :netcdf_version_id = "3.4" ;
#>                 :product_version = "2.0" ;
#>                 :creation_time = "2017-280T08:26:34.000" ;
#>                 :time_resolution = "daily" ;
#>                 :grid = "NSIDC" ;
#>                 :pole = "south" ;
#>                 :spatial_resolution = "12.5 km" ;
#>                 :platform_id = "F18" ;
#>                 :instrument = "SSM/I" ;
```

Using `ncdump` at the command line on a suitable system would yield very similar output to the print above.  

```bash 
ncdump -h /path/to/extdata/ifremer/20171002.nc
```

With the ncdf4 package it's a slightly different approach, but gives the same result. 

```R
print(ncdf4::nc_open(f))
```

Notice how the listing above is organized by *dimension* and then by *variable*. It's not particularly 
obvious that some variables are defined within the same set of dimensions as others. 

A NetCDF file is a container for simple array based data structures. There is [limited capacity][random-access] in the formal API for accessing data randomly within a variable, the primary mechanism is to define offset and stride (start and count) hyperslab indexes. 

[random-access]: I.e. it's not possible to query a file for an arbitrary sparse set of values, without constructing a degenerate hyperslab query for each point or reading a hyperslab containing cells not in the set.  Do you know different? Please let me know!

## tidync

Tidync aims to ease exploration of the contents of a NetCDF file and provides methods extract arbitrary hyperslabs. These can be used directly in array contexts, or in "long form" database contexts. 

On first contact with the file, the available variables are classified by grid and
dimension.  The "active" grid is the one that queries may be made against, and may be changed with the `activate` function. 



```r
library(tidync)
tidync(f)
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag    **ACTIVE GRID** ( 419648  values per variable)
#> [2]   D2       : time
#> 
#> Dimensions 3 (all active): 
#>   
#>   dim   name  length     min     max start count    dmin    dmax unlim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <int> <int>   <dbl>   <dbl> <lgl> 
#> 1 D0    ni       632 1032204 1032204     1   632 1032204 1032204 FALSE 
#> 2 D1    nj       664       1     632     1   664       1     632 FALSE 
#> 3 D2    time       1       1     664     1     1       1     664 FALSE 
#> # … with 1 more variable: coord_dim <lgl>
```

Here we see variables are clearly grouped by the *grid* they exist in, where grid is a specific (and ordered!) set of dimensions. This allows us to see the set of variables that implicitly co-exist, they have the same *shape*.  The first grid "D0,D1,D2" has two variables, *concentration* and *quality_flag*, and the second "D2" has only one variable *time*. There are no general rules here, a file might have any number of dimensions and variables, and any variable might be defined by one or more dimensions. 

In this case the D2 grid has only one variable in its single dimension, and it happens to be a special kind of variable - a "coordinate dimension", as indicated by the `coord_dim` flag. In the traditional `ncdump` summary above it's easy to see there's only really one data grid, in `ni,nj,time` that it holds two variables, and that time is a special coordinate dimension - in contrast neither `ni` or `nj` have
an explicit 1-dimension variable. When there are many dimensions and/or many variables those patterns are *not* easy to see. 

A particular grid was chosen by default, this is the "D0,D1,D2" grid composed of 3 dimensions, generally the largest grid will be chosen as that is *usually* the target we would be after. To choose a different grid we may nominate it by name, or by member variable. 

By name we choose a grid composed of only one dimension, and the summary print makes a distinction based on which dimensions are *active*. 


```r
tidync(f) %>% activate("D2")
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag
#> [2]   D2       : time    **ACTIVE GRID** ( 1  value per variable)
#> 
#> Dimensions 3 (1 active): 
#>   
#>   dim   name  length   min   max start count  dmin  dmax unlim coord_dim 
#>   <chr> <chr>  <dbl> <dbl> <dbl> <int> <int> <dbl> <dbl> <lgl> <lgl>     
#> 1 D2    time       1     1   664     1     1     1   664 FALSE TRUE      
#>   
#> Inactive dimensions:
#>   
#>   dim   name  length     min     max unlim coord_dim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <lgl> <lgl>     
#> 1 D0    ni       632 1032204 1032204 FALSE FALSE     
#> 2 D1    nj       664       1     632 FALSE FALSE
```

It is also possible to choose a grid by *variable name*, and in tidyverse style here we do not need to quote the name. If we choose a variable in the default grid, then it's no different to the first case. 


```r
tidync(f) %>% activate(time)
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag
#> [2]   D2       : time    **ACTIVE GRID** ( 1  value per variable)
#> 
#> Dimensions 3 (1 active): 
#>   
#>   dim   name  length   min   max start count  dmin  dmax unlim coord_dim 
#>   <chr> <chr>  <dbl> <dbl> <dbl> <int> <int> <dbl> <dbl> <lgl> <lgl>     
#> 1 D2    time       1     1   664     1     1     1   664 FALSE TRUE      
#>   
#> Inactive dimensions:
#>   
#>   dim   name  length     min     max unlim coord_dim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <lgl> <lgl>     
#> 1 D0    ni       632 1032204 1032204 FALSE FALSE     
#> 2 D1    nj       664       1     632 FALSE FALSE

## choose grid by variable name, which happens to be the default grid here
tidync(f) %>% activate(quality_flag)
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag    **ACTIVE GRID** ( 419648  values per variable)
#> [2]   D2       : time
#> 
#> Dimensions 3 (all active): 
#>   
#>   dim   name  length     min     max start count    dmin    dmax unlim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <int> <int>   <dbl>   <dbl> <lgl> 
#> 1 D0    ni       632 1032204 1032204     1   632 1032204 1032204 FALSE 
#> 2 D1    nj       664       1     632     1   664       1     632 FALSE 
#> 3 D2    time       1       1     664     1     1       1     664 FALSE 
#> # … with 1 more variable: coord_dim <lgl>

## same as the default
tidync(f) %>% activate("D0,D1,D2")
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag    **ACTIVE GRID** ( 419648  values per variable)
#> [2]   D2       : time
#> 
#> Dimensions 3 (all active): 
#>   
#>   dim   name  length     min     max start count    dmin    dmax unlim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <int> <int>   <dbl>   <dbl> <lgl> 
#> 1 D0    ni       632 1032204 1032204     1   632 1032204 1032204 FALSE 
#> 2 D1    nj       664       1     632     1   664       1     632 FALSE 
#> 3 D2    time       1       1     664     1     1       1     664 FALSE 
#> # … with 1 more variable: coord_dim <lgl>
```

The data variables available on a grid can be expanded out as a single data frame, which all the coordinates copied out - this is not efficient(!) but if we craft our queries sensibly to read only what we need, it's a very easy way to explore the data in a file. 

The 'hyper_filter' function allows specification of expressions to subset a variable based on each dimension's coordinate values.  

If no expressions are included we are presented with a table containing a row for each dimension, its extent in coordinates and its length. For convenience we also assign the activate form to an R variable, though we could just chain the entire operation without this. 


```r
concentration <- tidync(f) 
concentration %>% hyper_filter() 
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag    **ACTIVE GRID** ( 419648  values per variable)
#> [2]   D2       : time
#> 
#> Dimensions 3 (all active): 
#>   
#>   dim   name  length     min     max start count    dmin    dmax unlim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <int> <int>   <dbl>   <dbl> <lgl> 
#> 1 D0    ni       632 1032204 1032204     1   632 1032204 1032204 FALSE 
#> 2 D1    nj       664       1     632     1   664       1     632 FALSE 
#> 3 D2    time       1       1     664     1     1       1     664 FALSE 
#> # … with 1 more variable: coord_dim <lgl>
```


By specifying inequality expressions we see an *implicit* subsetting of the array. Everything so far is implicit to 
delay any file-based computation required to actually interact with the file and read from it. 

Notice that these are "name = expr" paired expressions, because the right hand side may be quite general we 
need the left hand side name to be assured of the name of the dimension referred to. 


```r

concentration %>% hyper_filter(nj = nj < 20)
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag    **ACTIVE GRID** ( 419648  values per variable)
#> [2]   D2       : time
#> 
#> Dimensions 3 (all active): 
#>   
#>   dim   name  length     min     max start count    dmin    dmax unlim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <int> <int>   <dbl>   <dbl> <lgl> 
#> 1 D0    ni       632 1032204 1032204     1   632 1032204 1032204 FALSE 
#> 2 D1    nj       664       1     632     1    19       1     632 FALSE 
#> 3 D2    time       1       1     664     1     1       1      19 FALSE 
#> # … with 1 more variable: coord_dim <lgl>
```

We can also use the special internal variable 'index', which will test against position in the dimension elements '1:length' rather than the values. It's not different in this case because ni and nj are just position dimensions anyway. The special 'dplyr' adverbs like 'between' will work. 


```r
concentration %>% hyper_filter(ni = index < 20, nj = dplyr::between(index, 30, 100))
#> 
#> Data Source (1): 20171002.nc ...
#> 
#> Grids (2) <dimension family> : <associated variables> 
#> 
#> [1]   D0,D1,D2 : concentration, quality_flag    **ACTIVE GRID** ( 419648  values per variable)
#> [2]   D2       : time
#> 
#> Dimensions 3 (all active): 
#>   
#>   dim   name  length     min     max start count    dmin    dmax unlim 
#>   <chr> <chr>  <dbl>   <dbl>   <dbl> <int> <int>   <dbl>   <dbl> <lgl> 
#> 1 D0    ni       632 1032204 1032204     1    19 1032204 1032204 FALSE 
#> 2 D1    nj       664       1     632    30    71       1      19 FALSE 
#> 3 D2    time       1       1     664     1     1      30     100 FALSE 
#> # … with 1 more variable: coord_dim <lgl>
```

## Data extraction

How to use these idioms to extract actual data? 

We can now exercise these variable choice and dimension filters to return actual data, either in by slicing out a  "slab" in array-form, or as a data frame. 


```r
hf <- concentration %>% hyper_filter(ni = index < 20, nj = dplyr::between(index, 30, 100))

## as an array
arr <- hf %>% hyper_array()
str(arr)
#> List of 2
#>  $ concentration: int [1:19, 1:71] NA NA NA NA NA NA NA NA NA NA ...
#>  $ quality_flag : int [1:19, 1:71] 8 8 8 8 8 8 8 8 8 8 ...
#>  - attr(*, "transforms")=List of 3
#>   ..$ ni  :Classes 'tbl_df', 'tbl' and 'data.frame':	632 obs. of  6 variables:
#>   .. ..$ ni       : int [1:632] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..$ index    : int [1:632] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..$ id       : num [1:632] 0 0 0 0 0 0 0 0 0 0 ...
#>   .. ..$ name     : chr [1:632] "ni" "ni" "ni" "ni" ...
#>   .. ..$ coord_dim: logi [1:632] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   .. ..$ selected : logi [1:632] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>   ..$ nj  :Classes 'tbl_df', 'tbl' and 'data.frame':	664 obs. of  6 variables:
#>   .. ..$ nj       : int [1:664] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..$ index    : int [1:664] 1 2 3 4 5 6 7 8 9 10 ...
#>   .. ..$ id       : num [1:664] 1 1 1 1 1 1 1 1 1 1 ...
#>   .. ..$ name     : chr [1:664] "nj" "nj" "nj" "nj" ...
#>   .. ..$ coord_dim: logi [1:664] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   .. ..$ selected : logi [1:664] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   ..$ time:Classes 'tbl_df', 'tbl' and 'data.frame':	1 obs. of  6 variables:
#>   .. ..$ time     : num 1032204
#>   .. ..$ index    : int 1
#>   .. ..$ id       : num 2
#>   .. ..$ name     : chr "time"
#>   .. ..$ coord_dim: logi TRUE
#>   .. ..$ selected : logi TRUE

## as a data frame

hf %>% hyper_tibble() %>% dplyr::filter(!is.na(concentration))
#> # A tibble: 0 x 5
#> # … with 5 variables: concentration <int>, quality_flag <int>, ni <int>,
#> #   nj <int>, time <dbl>
```

## Helper functions

Further utilities are available to get immediate information on the current status of the activated grid, and the subsetted space. These
are directly useable by the traditional API tools, and in particular by the functions `ncdf4::ncvar_get()` (`varid`, `start`, `count`), its counterpart in `RNetCDF::var.get.nc()` and command line tools like CDO. 


```r
hyper_vars(hf)
#> # A tibble: 2 x 6
#>      id name          type    ndims natts dim_coord
#>   <dbl> <chr>         <chr>   <dbl> <dbl> <lgl>    
#> 1     1 concentration NC_BYTE     3     6 FALSE    
#> 2     2 quality_flag  NC_BYTE     3     6 FALSE
hyper_dims(hf)
#> # A tibble: 3 x 7
#>   name  length start count    id unlim coord_dim
#>   <chr>  <dbl> <int> <int> <dbl> <lgl> <lgl>    
#> 1 ni       632     1    19     0 FALSE FALSE    
#> 2 nj       664    30    71     1 FALSE FALSE    
#> 3 time       1     1     1     2 FALSE TRUE
axis_transforms(hf)
#> $time
#> # A tibble: 1 x 6
#>      time index    id name  coord_dim selected
#>     <dbl> <int> <dbl> <chr> <lgl>     <lgl>   
#> 1 1032204     1     2 time  TRUE      TRUE    
#> 
#> $ni
#> # A tibble: 632 x 6
#>       ni index    id name  coord_dim selected
#>    <int> <int> <dbl> <chr> <lgl>     <lgl>   
#>  1     1     1     0 ni    FALSE     TRUE    
#>  2     2     2     0 ni    FALSE     TRUE    
#>  3     3     3     0 ni    FALSE     TRUE    
#>  4     4     4     0 ni    FALSE     TRUE    
#>  5     5     5     0 ni    FALSE     TRUE    
#>  6     6     6     0 ni    FALSE     TRUE    
#>  7     7     7     0 ni    FALSE     TRUE    
#>  8     8     8     0 ni    FALSE     TRUE    
#>  9     9     9     0 ni    FALSE     TRUE    
#> 10    10    10     0 ni    FALSE     TRUE    
#> # … with 622 more rows
#> 
#> $nj
#> # A tibble: 664 x 6
#>       nj index    id name  coord_dim selected
#>    <int> <int> <dbl> <chr> <lgl>     <lgl>   
#>  1     1     1     1 nj    FALSE     TRUE    
#>  2     2     2     1 nj    FALSE     TRUE    
#>  3     3     3     1 nj    FALSE     TRUE    
#>  4     4     4     1 nj    FALSE     TRUE    
#>  5     5     5     1 nj    FALSE     TRUE    
#>  6     6     6     1 nj    FALSE     TRUE    
#>  7     7     7     1 nj    FALSE     TRUE    
#>  8     8     8     1 nj    FALSE     TRUE    
#>  9     9     9     1 nj    FALSE     TRUE    
#> 10    10    10     1 nj    FALSE     TRUE    
#> # … with 654 more rows
hyper_dims(hf)
#> # A tibble: 3 x 7
#>   name  length start count    id unlim coord_dim
#>   <chr>  <dbl> <int> <int> <dbl> <lgl> <lgl>    
#> 1 ni       632     1    19     0 FALSE FALSE    
#> 2 nj       664    30    71     1 FALSE FALSE    
#> 3 time       1     1     1     2 FALSE TRUE
hyper_tbl_cube(hf)
#> Warning in hyper_slice(x, ...): hyper_array should be used instead of
#> hyper_slice
#> Source: local array [1,349 x 3]
#> D: ni [int, 19]
#> D: nj [int, 71]
#> D: time [dbl, 1]
#> M: concentration [int]
#> M: quality_flag [int]
hyper_vars(hf)
#> # A tibble: 2 x 6
#>      id name          type    ndims natts dim_coord
#>   <dbl> <chr>         <chr>   <dbl> <dbl> <lgl>    
#> 1     1 concentration NC_BYTE     3     6 FALSE    
#> 2     2 quality_flag  NC_BYTE     3     6 FALSE

active(hf)
#> [1] "D0,D1,D2"

#activate(hf)
```


## Future improvements

There are many improvements that could be made here, but so far attempting
them has complicated matters rather than improving them so I simply list them. 

* support multiple sources at once for lazy read of a virtual grid
* support groups
* support compound types
* allow a group-by function for a polygon layer, against a pair of dimensions to classify cells
* allow a truly DBI and dplyr level of lazy read, with more filter, select, mutate and collect idioms
* provide converters to raster format, stars format


[not-a-format]: https://twitter.com/TedHabermann/status/958034585002041344
