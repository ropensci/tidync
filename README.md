
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidync

<!-- badges: start -->

[![](https://badges.ropensci.org/174_status.svg)](https://github.com/ropensci/software-review/issues/174)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tidync)](https://cran.r-project.org/package=tidync)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/tidync)](https://cran.r-project.org/package=tidync)
[![R-CMD-check](https://github.com/ropensci/tidync/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/tidync/actions)
<!-- badges: end -->

The goal of tidync is to ease exploring the contents of a NetCDF source
and to simplify the process of data extraction.

When extracting, data can be accessed as array/s, or in long-form as a
data frame. In contrast to other packages tidync helps reduce the volume
of code required to discover and read the contents of NetCDF, with
simple steps:

-   Connect and summarize `tidync()`.
-   (optionally) Specify source variables `activate()`.
-   (optionally) Specify array sub-setting (slicing) `hyper_filter()`.
-   Read array data in native form `hyper_array()` or long-form
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

More general information about system dependencies libnetcdf-dev and
libudunits2-dev is available from
[Unidata](https://www.unidata.ucar.edu).

## Usage

This is a basic example which shows how to connect to a file.

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

### Interactive

Use `tidync()` and `hyper_filter()` to discern what variables and
dimensions are available, and to craft axis-filtering expressions by
value or by index. (Use the name of the variable on the LHS to target
it, use its name to filter by value and the special name `index` to
filter it by its index).

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
#>   dim   name      length   min   max start count  dmin  dmax unlim coord_dim 
#>   <chr> <chr>      <dbl> <dbl> <dbl> <int> <int> <dbl> <dbl> <lgl> <lgl>     
#> 1 D7    STRING256    256     1   256     1   256     1   256 FALSE FALSE     
#> 2 D8    N_PROF         2     1     2     1     2     1     2 FALSE FALSE     
#> 3 D9    N_PARAM        7     1     7     1     7     1     7 FALSE FALSE     
#> 4 D11   N_CALIB        1     1     1     1     1     1     1 FALSE FALSE     
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
#> Warning in hyper_filter.tidync(., N_PROF = N_PROF > 1, STRING256 = index > :
#> 'STRING256' not found in active grid, ignoring
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
#> # A tibble: 493 × 37
#>     PRES PRES_QC PRES_AD…¹ PRES_…² PRES_…³  TEMP TEMP_QC TEMP_…⁴ TEMP_…⁵ TEMP_…⁶
#>    <dbl> <chr>       <dbl> <chr>     <dbl> <dbl> <chr>     <dbl> <chr>     <dbl>
#>  1  7.70 1            7.79 1          2.40  13.2 1          13.2 1       0.00200
#>  2 11.8  1           11.9  1          2.40  13.2 1          13.2 1       0.00200
#>  3 16.3  1           16.4  1          2.40  13.2 1          13.2 1       0.00200
#>  4 21.6  1           21.7  1          2.40  13.2 1          13.2 1       0.00200
#>  5 26.7  1           26.8  1          2.40  13.2 1          13.2 1       0.00200
#>  6 31.7  1           31.8  1          2.40  13.2 1          13.2 1       0.00200
#>  7 36.6  1           36.7  1          2.40  13.2 1          13.2 1       0.00200
#>  8 41.4  1           41.5  1          2.40  13.2 1          13.2 1       0.00200
#>  9 46.5  1           46.6  1          2.40  13.2 1          13.2 1       0.00200
#> 10 51.8  1           51.9  1          2.40  13.2 1          13.2 1       0.00200
#> # … with 483 more rows, 27 more variables: PSAL <dbl>, PSAL_QC <chr>,
#> #   PSAL_ADJUSTED <dbl>, PSAL_ADJUSTED_QC <chr>, PSAL_ADJUSTED_ERROR <dbl>,
#> #   DOXY <dbl>, DOXY_QC <chr>, DOXY_ADJUSTED <dbl>, DOXY_ADJUSTED_QC <chr>,
#> #   DOXY_ADJUSTED_ERROR <dbl>, CHLA <dbl>, CHLA_QC <chr>, CHLA_ADJUSTED <dbl>,
#> #   CHLA_ADJUSTED_QC <chr>, CHLA_ADJUSTED_ERROR <dbl>, BBP700 <dbl>,
#> #   BBP700_QC <chr>, BBP700_ADJUSTED <dbl>, BBP700_ADJUSTED_QC <chr>,
#> #   BBP700_ADJUSTED_ERROR <dbl>, NITRATE <dbl>, NITRATE_QC <chr>, …
subs %>% hyper_tbl_cube(select_var = c("PRES", "PRES_QC", "PSAL_ADJUSTED"))
#> $mets
#> Class: tidync_data (list of tidync data arrays)
#> Variables (3): 'PRES', 'PRES_QC', 'PSAL_ADJUSTED'
#> Dimension (1): N_LEVELS,N_PROF (493)
#> Source: /perm_storage/home/mdsumner/R/x86_64-pc-linux-gnu-library/4.2/tidync/extdata/argo/MD5903593_001.nc
#> 
#> $dims
#> $dims$N_LEVELS
#>   [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
#>  [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
#>  [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
#>  [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
#>  [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
#>  [91]  91  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108
#> [109] 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126
#> [127] 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144
#> [145] 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 160 161 162
#> [163] 163 164 165 166 167 168 169 170 171 172 173 174 175 176 177 178 179 180
#> [181] 181 182 183 184 185 186 187 188 189 190 191 192 193 194 195 196 197 198
#> [199] 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216
#> [217] 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234
#> [235] 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252
#> [253] 253 254 255 256 257 258 259 260 261 262 263 264 265 266 267 268 269 270
#> [271] 271 272 273 274 275 276 277 278 279 280 281 282 283 284 285 286 287 288
#> [289] 289 290 291 292 293 294 295 296 297 298 299 300 301 302 303 304 305 306
#> [307] 307 308 309 310 311 312 313 314 315 316 317 318 319 320 321 322 323 324
#> [325] 325 326 327 328 329 330 331 332 333 334 335 336 337 338 339 340 341 342
#> [343] 343 344 345 346 347 348 349 350 351 352 353 354 355 356 357 358 359 360
#> [361] 361 362 363 364 365 366 367 368 369 370 371 372 373 374 375 376 377 378
#> [379] 379 380 381 382 383 384 385 386 387 388 389 390 391 392 393 394 395 396
#> [397] 397 398 399 400 401 402 403 404 405 406 407 408 409 410 411 412 413 414
#> [415] 415 416 417 418 419 420 421 422 423 424 425 426 427 428 429 430 431 432
#> [433] 433 434 435 436 437 438 439 440 441 442 443 444 445 446 447 448 449 450
#> [451] 451 452 453 454 455 456 457 458 459 460 461 462 463 464 465 466 467 468
#> [469] 469 470 471 472 473 474 475 476 477 478 479 480 481 482 483 484 485 486
#> [487] 487 488 489 490 491 492 493
#> 
#> $dims$N_PROF
#> [1] 2
#> 
#> 
#> attr(,"class")
#> [1] "tbl_cube"
```

A grid is a “virtual table” in the sense of a database source. It’s
possible to activate a grid via a variable within it, so all variables
are available by default. Grids have identifiers based on which
dimensions they are defined with, so use i.e. “D1,D0” and can otherwise
be activated by their count identifier (starting at 1). The “D0” is an
identifier, it matches the internal 0-based indexing and identity used
by NetCDF itself.

Please note that `hyper_filter()` expressions must be unique, unlike
with `dplyr::filter()` we cannot load multiple comparisons into one.

While dplyr filter can load up multiple comparisons:

``` r
df %>% dplyr::filter(longitude > 100, longitude < 150)
```

in hyper_filter we must load them into one named expression.

``` r
tidync(filename) %>% hyper_filter(longitude = longitude > 100 & longitude < 150)
```

### Extractive

Use what we learned interactively to extract the data, either in data
frame or raw-array (hyper slice) form.

``` r
## we'll see a column for the variable activated, and whatever other 
## variables the grid has
tidync(filename) %>% activate("JULD") %>% 
  hyper_filter(N_PROF = N_PROF == 1) %>% 
  hyper_tibble()
#> # A tibble: 1 × 2
#>     JULD N_PROF
#>    <dbl>  <int>
#> 1 22719.      1


## native array form, we'll see a (list of) R arrays with a dimension for 
## each seen by tidync(filename) %>% activate("JULD")
tidync(filename) %>% activate("JULD") %>% 
  hyper_filter(N_PROF = N_PROF == 1) %>% 
  hyper_array()
#> Class: tidync_data (list of tidync data arrays)
#> Variables (1): 'JULD'
#> Dimension (1): N_PROF (1)
#> Source: /perm_storage/home/mdsumner/R/x86_64-pc-linux-gnu-library/4.2/tidync/extdata/argo/MD5903593_001.nc
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
providing. There’s a lot of room for improvement!

-   we can’t do “grouped filters”” (i.e. polygon-overlay extraction),
    but it’s in the works
-   compound types are not supported, though see the “rhdf5” branch on
    Github
-   NetCDF groups are not exposed (groups are like a “files within a
    file”, analogous to a file system directory)

I’m interested in lighter and rawer access to the NetCDF library, I’ve
explored that here and it may or may not be a good idea:

<https://github.com/hypertidy/ncapi>

## Terminology

-   **slab**, **hyperslab** - array variable that may be read from a
    NetCDF
-   **shape**, **grid** - set of dimensions that define variables in
    NetCDF
-   **activation** - choice of a given grid to apply subsetting and read
    operations to

------------------------------------------------------------------------

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/ropensci/tidync/blob/master/CONDUCT.md). By
participating in this project you agree to abide by its terms.

[![ropensci_footer](https://ropensci.org/public_images/ropensci_footer.png)](https://ropensci.org)
