
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
file <- system.file("inst", "unidata", "madis-hydro.nc", package = "tidync")
```

See this article for more:

Code of conduct
---------------

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
