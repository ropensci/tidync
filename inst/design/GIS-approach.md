
I'd like to see a simple and abstract new feature for NetCDF that simply provided a table for each dimension in a NetCDF source, an "axis table" if you will that provides a light window into the potentially massive grids of data inhabited in sets of those axes. 

NetCDF files are a very general way of storing array-based data. It doesn't fit cleanly into a GIS-raster context. It has three main entities: variables, dimensions, attributes. 

There's no single convention for the interpretation of the entities in NetCDF, though there are several standards that provide predictable interpretations - but any software has to choose its relationship to them. 

Any array in a NetCDF souce may exist within one or more of several dimension within the file. The dimensions are defined independently of the array data, they may be geographic, or completely abstract - and there's no reason not to have a "geographic" time series store in Time, Y, X order - the dimension orientation is chosen to optimize downstream extraction or to align with future data coming in. (A dimension can be "unlimited" which means a virtual array might spread its dimension slices across many files). 

A dimension is a one-dimensional axis, and will usually have a "coordinate array", so longitude is one example where the values are the "column positions" in geographic space. But, they might be without explicit coordinates and simply have an index. One or more of these dimensions defines a space, or grid in which one or more variables can exist. In this sense, any variables on the same grid are like fields in a table. 

The coordinate values might be completely regular, in which case they are "redundant" in the sense that they can be compressed to two numbers, the position of the first coordinate and the resolution (or step size in the axis space). What GDAL calls a "transform" is this offset/scale abstraction, the affine transform (shear components are never used in NetCDF afaik, but neither is the affine transform ever really used - one exception is the GMT variant of NetCDF files, from the Hawaii topography group. 

Another slight variant is that the extent of the axis might be stored, so it's truly an interval rather than a (possibly ambiguous) single coordinate for each "cell". 

The dimension's coordinates may also be "rectlinear", in the sense that they are not regularly spaced - they should at least be monotonic increasing or decreasing, but there's a wild zoo out there and users need to be able investigate these kind of problems and over ride interpretations some times.  

Many files that don't have explicit 1-D axis coordinates will store an array of coordinates as variables, so for example in ROMs 4-D model output there is an abstract model grid, and the longitude values are stored in one variable in that space (x_rho, eta_rho) and latitude values in another. Here the "geography" is actually just data, stored in the same pair of axes  as is used by the first two axes of the the 4D model data. 

There's a clear "database view" of these structures where by each axis is a table, with a specific number of rows, and fields index and coordinate, and in a table it means the coordinate might be implicit on the index (in the case of a dimension without coordinate values), and there's also room for other variants like a date-time type for the raw coordinate value. 

These tables can be selected and filtered with standard SQL and used to determine the start / count index calls that the NetCDF library needs to read data from a variable. On this basis I see a future Manifold that interprets a NetCDF in this very generic form, a form that allows delaying any explicit data read (the data might be coming in in future for example, or simply be fed from a short message subsetting a remote Thredds service) and providing a basis for a very powerful  user-custom development of tools that work for specific models. 

The affine maps delivered in NetCDF are really obvious and easy, but the curvlinear ones seem opportune for a smart database-like solution to ease the transition from a lot scientific work done on explicit arrays and only in reach of technical programmers. 

In one sense by ignoring the coordinate-scheme for dimensions any n-dimensional array may be seen as a set of unrolled 2D raster grids, but that's about as simple as it gets, and that is essentially what Manifold 8.0 does. GDAL is very similar, you get a "band" for every 2D slice in whatever dimensional array.  This is ok for getting out the data, but it gives very little attention to the generality and richness of the models and how the data was produced and intended to be used. 

I've written a variant of this "table of axes" approach in R and that process helped me to finally see that this format is like a dumbed down database, I think we could have a very powerful set of tools in Manifold/Radian that are based on this general interpretation and that allows users to  access these data independently of any "geographic" interpretation, and let them stream it into whatever process it is they need. There are a host of obvious applications for the easy things, like animated maps from climate model output, analysis of wind and current vector fields and so on that the Radian performance and future prospects for visualization would enable. 

 I think the user community can build a good understanding about this complex format if we start with a set of tables that represent the available axes in the source, and then explore how to use those to provide (subsettable) links into the raw data (which Radian could easily simply ingest in total for many sources). 
