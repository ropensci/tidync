# tidync dev

* Can now activate with a bare variable name. As per https://github.com/ropensci/tidync/issues/95


# tidync 0.2.3

* Remove test burden. 

# tidync 0.2.2

* Fix broken tests due to changes in RNetCDF, thanks to CRAN for notification. 

* Added example to readme and docs, and checks for non-unique `hyper_filter()` names, as per #93. 
 Thanks to @everydayduffy for the suggestion. 

# tidync 0.2.1

* Now depends on R 3.5.0. 

* Removed obscure numeric 'what' logic for `activate()`. 

* Fixed tidync method for `hyper_array()` output. 

* Found a really bad bug in `hyper_array()` (#92), now fixed. Axis order in 
  transforms was sometimes reversed, which caused garbled results from `hyper_tibble()`. 
  The effect is to ruin any ggplot2 figures for some source files. It's very likely
  that no other system yet uses the `hyper_array()` format so impact is low.  

# tidync 0.2.0

* tidync is now part of the rOpenSci project. 

* Fixes in tests and examples to avoid version-incapable NetCDF problems on Solaris. 
 
* A number of improved tests and documentation fixes. 

* Deal with warnings from tidyr version > 0.8.3. 

* A huge thank you to all rOpenSci contributions, especially very helpful reviews from   
  [&#x0040;Nowosad](https://github.com/Nowosad) and [&#x0040;timcdlucas](https://github.com/timcdlucas) as well as organizers [&#x0040;karthik](https://github.com/karthik), [&#x0040;stefaniebutland](https://github.com/stefaniebutland)  and [&#x0040;sckott](https://github.com/sckott). 
  
  Helpful input was also provided via issues  from [&#x0040;adrfantini](https://github.com/adrfantini), [&#x0040;JustBerkhout](https://github.com/JustBerkhout), [&#x0040;matteodefelice](https://github.com/matteodefelice), [&#x0040;rensa](https://github.com/rensa), 
  [&#x0040;rmendels](https://github.com/rmendels), [&#x0040;sw-rifai](https://github.com/sw-rifai), and [&#x0040;tremenyi](https://github.com/tremenyi). 
  
  Pull requests were contributed by [&#x0040;dlebauer](https://github.com/dlebauer), [&#x0040;edzer](https://github.com/edzer). 

# tidync 0.1.1

* Package improvements thanks to CRAN feedback, clarified Description and added
  more examples. Replaced `cat()` and `print()` calls with `message()` and `warning()`. 

* New class 'tidync_data' for output of `hyper_array()`, no underlying change to 
  the object which is simply a list of arrays from each variable, and axis transforms 
  stored in an attribute. 

* Old deprecated function `axis_transforms()` now Defunct. 


# tidync 0.1.0

* FIRST RELEASE, tidync was greatly improved via help from the rOpenSci review 
  process. 

* New function `hyper_grids()` to report available grid names. 

* A printing error of dimension value ranges is now fixed, thanks to James Goldie 
  (#84). 

* Now supports 'NC_CHAR' type, by exploding these into the array size expected. 

* Breaking change: when using `tidync$grid`it's now expected that this must be  
  `tidyr::unnest()`ed in order to expand out the grid list per variable, in line 
  with https://github.com/hypertidy/ncmeta/issues/26. 
  
* The `hyper_array` function now stores the relevant transforms table as an attribute 
  `transforms` so that objects can be constructed directly from the native array 
  output. 

# tidync 0.0.3

* Function rename `hyper_array()` now matches `hyper_tibble()` indicating the form 
  of the output (rather than the action used, was `hyper_slice()`). 

* New functions `hyper_vars()` and `hyper_dims()` for reporting on the currently active 
  variables and dimensions and their status. 

* Now dependent on ncmeta >= 0.0.2, partly to avoid crashing on invalid source 
  or file strings

* Removed `hyper_index()` and incorporated that into `hyper_filter()`, there's 
  now only one delay-capable class which is 'tidync'


# tidync 0.0.2

* Function `hyper_filter()` now uses a selection idiom, to record the state of 
  the axis rather than explicitly filter it. This means we can have more flexibility 
  on what the axis transform tables can be used for, and removes some unwieldy 
  handling code. All the available axes are on the object from first contact, which 
  means we can program against the entire space in the source which will help for 
  complex mapping scenarios. 
 
* Function `hyper filter()` print now handles the case of char-type coordinate 
  values by setting the min and max to NA_real_

* Various improvements and fixes for the print method for tidync

* Support coordinate-less dimensions has been added, there is new information in 
  the print summary about which dimensions are a "coord_dim" and this results in 
  the axis transform tables using the index as the coordinate value.  

* Functions `hyper_slice()` and `hyper_tibble()` now return all variables that 
  exist within a grid

* This version sees a new model where activation is on 'grids', effectively a 
  space composed of dimensions. In addition to the variables, dimension, and attributes 
  entities we add 'grid' defined by a set of dimensions, and 'axis' which is an 
  instance of a particular dimension as used by a variable. 

* Sources without recognizable variables now gracefully handled, with help from 
  ncmeta. 

# tidync 0.0.1

* Now imports ncdump > 0.0.3. 

* Installed external example data from [Unidata website](https://www.unidata.ucar.edu/software/netcdf/examples/files.html)

* First working version now has `tidync()`, and `hyper_*()` family of functions. 

* Migrated from ncdump. 


