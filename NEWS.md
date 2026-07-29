# tidync 0.5.0

This release adds multi-source support, request of (#131): a tidync object can now present a consolidated
lazy view across a collection of NetCDF sources, extending the
filter-then-read model unchanged from one file to many.

## Multi-source collections

* `tidync()` now accepts a vector of file paths together with a
  `concat_dim` argument, building a consolidated view across sources along
  that dimension (typically time). Downstream operations (`hyper_filter()`,
  `hyper_array()`, `hyper_tibble()`, `hyper_tbl_cube()`) work transparently
  across the collection, and only the files needed for the current
  selection are opened at read time. A single file with `concat_dim` is a
  valid degenerate collection.

* Fast mode for large collections:
  `tidync(files, concat_dim = "time", fast = TRUE)` skips full metadata
  scans of sources 2..N, reading only the concat dimension coordinate from
  each file. Validation is deferred to data-read time, where dimension
  mismatches are reported with the offending file named. In fast mode the
  time coordinate is presented numerically (no timestamp column), since
  per-source calendar metadata is not read; use the default full mode for
  CFtime timestamps.

* Zero file I/O construction: supply coordinate values directly with
  `concat_dim = list(name = "time", values = dates)` and only the first
  source is opened (as the template). Values can be numeric, Date, or
  POSIXct, and `hyper_filter()` operates on them directly. This is aimed
  at file-database workflows (for example raadfiles) where per-file
  coordinates are already known. This construction assumes one step per
  source; files holding more steps on the concat dimension are detected at
  read time and reported as an error rather than silently subset.

* Read-time validation is armed automatically for fast mode and for
  values-supplied construction: shared dimensions and the per-source
  concat dimension length are checked against the consolidated view as
  each file is opened, with clear errors naming the file.

* Optional parallel reads via mirai: when `mirai::daemons()` are active,
  per-source reads in `hyper_array()` run in parallel with
  `mirai::mirai_map()`, falling back to sequential reads when mirai is not
  installed or no daemons are set. Parallelism is entirely under user
  control; tidync never starts daemons itself. Errors raised during
  parallel reads (including validation failures) propagate normally.

* Sources are kept in the order supplied; the consolidated `index` on the
  concat dimension is file order. Value-based filters work regardless of
  ordering, but supply sources in coordinate order for a monotonic view.

* `tidync()` on a `tidync_data` object (round-trip) preserves the source
  table and `concat_dim`.

## Known limitations

* Non-contiguous selections *within* a single source still read a
  bounding slab and are not supported at `hyper_array()` time, consistent
  with single-source behaviour. Non-contiguous selections that span whole
  sources (for example dropping intermediate files) are supported and read
  only the files required.

## Fixes

* Fixed `hyper_tibble()` producing constant dimension values when
  `drop = TRUE` (the default) collapses arrays to bare vectors;
  expansion is now based on `length()` rather than `prod(dim())`.

* Fixed the print method showing NA for min/max of Date/POSIXct
  coordinate columns; non-numeric coordinates are now converted with
  `as.numeric()` (Date as days since epoch) rather than replaced with NA.

* Repaired mangled pipe operators in the package overview help examples.

* Removed commented-out `browser()` calls.

* Update to CFTime as_timestamp(), thanks to @fabern. 

* Support for CF time metadata via package CFtime thanks to @pvanlaake, see https://github.com/ropensci/tidync/pull/124

* Suppress ncdf4 warnings on open, issue #119. 


## Dependencies and internals

* Minimum R version is now 4.1.0 (native pipe `|>`). Removed forcats,
  magrittr, and purrr dependencies; `%>%` replaced with `|>` throughout
  and `purrr::safely()` replaced with `tryCatch()`.

* ncmeta (>= 0.5.0) is now required, for extended (CFtime) metadata and
  for correct handling of sources with no variables (such as compound-type
  L3 bin files), which errored uninformatively with earlier ncmeta under
  current dplyr.

* Minimum dplyr version is 1.1.0 (`multiple` argument in joins) and
  minimum tidyr version is 1.0.0 (`cols` argument in `unnest()`); all
  version-gating conditionals removed.

* mirai added to Suggests.

* `print.tidync` now returns its input invisibly, as always claimed. 


# tidync 0.3.0

* Add option to silence all warnings and messages. See #115, suggestion of @markpayneatwork. 


# tidync 0.2.5

* Improved documentation for large data check in `hyper_array()` and `hyper_tibble()` and `hyper_tbl_cube()`, 
and allowed user-controlled option to avoid this check. Thanks to Alessandro Bigi for feedback. 

# tidync 0.2.4

* Now inports cubelyr rather than dplyr for `tbl_cube()` thanks to Hadley Wickham PR #102. 

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

* Installed external example data from [Unidata website](https://www.unidata.ucar.edu/software/netcdf)

* First working version now has `tidync()`, and `hyper_*()` family of functions. 

* Migrated from ncdump. 


