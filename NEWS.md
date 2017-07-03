# tidync dev

* various improvements and fixes for the print method for tidync

* support coordinate-less dimensions has been added, there is new information in the print summary about which dimensions are a "coord_dim" and this results in the axis transform tables using the index as the 'coordinate value' 

* hyper_slice and hyper_tibble now return all variables that exist within a grid

* This version sees a new model is applied where activation is on 'grids', effectively a 'space' composed of dimensions. In addition to the variables, dimension, attributes we add "grid" defined by a set of dimensions, and "axis" which is an instance of a particular dimension as used by a variable. 

* files without recognizable variables now gracefully handled, with help from ncmeta

# tidync 0.0.1

* imports ncdump > 0.0.3

* Installed external example data from [Unidata website](https://www.unidata.ucar.edu/software/netcdf/examples/files.html)

* First working version now has 'tidync', and 'hyper_' family of functions. 

* Migrated from ncdump. 


