
#' tidy netcdf
#' 
#' Function to extract all metadata from a NetCDF, for use in subsequent operations. By default
#' the first variable encountered is  `activate`d. 
#' 
#' Any NetCDF with variable arrays should work. Files with compound types are not yet supported. We
#' haven't explored HDF5 per se, so any feedback is appreciated. 
#' @param x path to a NetCDF file
#' @param what (optional) character or bare name of variable to `activate`
#' @export
tidync <- function(x, what) {
  ## TODO support NetCDF, ncdf4, RNetCDF, raster, anything with a file behind it
  if (!is.character(x)) stop("'x' must be a file")
  fexists <- file.exists(x)
  if (!fexists) stop(sprintf("cannot find file: \n%s", x))
  x <- structure(unclass(ncdump::NetCDF(x)), class = "tidync")
  if (missing(what)) what <- x$variable$name[1L]
  activate(x, what)
}

#' hyper tibble
#'
#' @param x object to tibbulate
#' @param ... arguments to `hyper_filter``
#'
#' @return a `tbl_df`
#' @export
#' @importFrom dplyr %>% 
#' @export %>% 
#' @examples
#' l3file <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump")
#' rnc <- tidync(l3file)
#' hyper_filter(rnc)
#' library(dplyr)
#' hyper_slice(l3file, lat = lat > 0) %>% dim()
#' 
#'  ht <- hyper_tibble(rnc) %>% filter(!is.na(chlor_a)) 
#' ht   
#' library(ggplot2)
#' ggplot(ht %>% filter(!is.na(chlor_a)), 
#' aes(x = lon, y = lat, fill = chlor_a)) + geom_point()
#' 
hyper_tibble <- function(x, ...) {
  UseMethod("hyper_tibble")
}
#' @name hyper_tibble
#' @importFrom ncdump NetCDF
#' @export
hyper_tibble.character <- function(x, ...) {
  tidync(x) %>% hyper_filter(...) %>% hyper_tibble()
}
#' @name hyper_tibble
#' @export
hyper_tibble.tidync <- function(x, ...) {
  x %>% hyper_filter(...) %>% hyper_tibble()
}
#' @name hyper_tibble
#' @export
hyper_tibble.hyperfilter <- function(x, ...) {
  slab <- hyper_slice(x, ...)
  tib <- list()
  tib[[active(x)]] <- as.vector(slab)
  tib <- tibble::as_tibble(tib)
  prod_dims <- 1
  total_prod <- prod(dim(slab))
  
  
  for (i in seq_along(x)) {
    nm <- names(x)[i]
    nr <- nrow(x[[i]])
    tib[[nm]] <- rep(x[[nm]][[nm]], each = prod_dims, length.out = total_prod)
    prod_dims <- prod_dims * nr
  }
  tib
  
}

#' Array subset by nse
#'
#' NSE arguments must be named as per the dimensions in the variable. This is a restrictive variant of `dplyr::filter`, 
#' with a syntax more like `dplyr::mutate`. This ensures that each element is named, so we know which dimension to 
#' apply this to, but also that the expression evaluated against can do some extra work for a nuanced test. 
#' @param x NetCDF object
#' @param ... currently ignored
#'
#' @return data frame
#' @export
#' @importFrom purrr map
#' @importFrom dplyr group_by mutate summarize
#' @examples
#' ## inst/dev/filtrate_early.R
#' ##http://rpubs.com/cyclemumner/tidyslab
#' # 
hyper_filter <- function(x, ...) {
  UseMethod("hyper_filter")
}
#' @name hyper_filter
#' @export
#' @importFrom dplyr %>% mutate 
#' @importFrom forcats as_factor
hyper_filter.tidync <- function(x, ...) {
  
  dimvals <- dimension_values(x) #%>% 
  dimvals$step <- unlist(lapply(split(dimvals, forcats::as_factor(dimvals$name)), function(x) seq_len(nrow(x))))
  trans <-  split(dimvals, forcats::as_factor(dimvals$name)) 
  
  ## hack attack
  for (i in seq_along(trans)) names(trans[[i]]) <- gsub("^vals$", trans[[i]]$name[1], names(trans[[i]]))
  
  quo_named <- rlang::quos(...)
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100'")
  quo_noname <- unname(quo_named)
  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    trans[[iname]] <- dplyr::filter(trans[[iname]], !!!quo_noname[i])
    
  }
  trans <- lapply(trans, function(ax) {ax$filename <- x$file$filename; ax})
  hyper_filter(trans) %>% activate(active(x))
  
}

#' @name hyper_filter
#' @export
hyper_filter.default <- function(x, ...) {
  structure(x, class = c("hyperfilter", class(x)))
}
#' @name hyper_filter
#' @export
hyper_filter.character <- function(x, ...) {
  tidync(x) %>% hyper_filter(...)
}
#' @name hyper_filter
#' @export
hyper_filter.hyperfilter <- function(x, ...) {
  stop("too many filters in the chain, you can't (yet) 'hyper_filter' a hyperfilter")
}
#' @name hyper_filter
#' @importFrom dplyr bind_rows funs group_by select summarize_all
#' @export
print.hyperfilter <- function(x, ...) {
  x <- dplyr::bind_rows(lapply(x,  function(a) dplyr::summarize_all(a %>% 
     dplyr::select(-.data$filename, -.data$.dimension_, -.data$id, -.data$step) %>% 
                                                        group_by(.data$name), dplyr::funs(min, max, length))))
  print("filtered dimension summary: ")
  print(x)
  invisible(x)
}

 hyper_filter_Spatial <- function(x, y, ...) {
   #stop("nothing to see here")
   #warning("assuming first two dimensions are longitude-latitude ...")
   #dim_tabs <- hyper_filter(x)
   #xy_names <- names(dim_tabs)[1:2]
   #xy_grid <- as.matrix(expand.grid(x = dim_tabs[[1]][[xy_names[1]]], 
  #                        y = dim_tabs[[1]][[xy_names[1]]]))
   #over(y, SpatialPoints(xy_grid, proj4string = crs(y)))
 }

#' Activate
#'
#' Set the context for subsequent manipulations. 
#' 
#' `activate` puts the named variable first
#' `active` gets and sets the active variable
#' @param .data NetCDF object
#' @param what name of a variable
#' @return NetCDF object
#' @importFrom activate activate active active<- 
#' @export active activate active<- 
#' @rdname activate 
#' @aliases active activate active<- 
#' @examples
#' l3file <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump")
#' rnc <- tidync(l3file)
#' activate(rnc, "palette")
#' @name activate
#' @export
activate <- function(.data, what) UseMethod("activate")
#' @name activate
#' @export
activate.tidync <- function(.data, what) {
  what_name <- deparse(substitute(what))
  if (what_name %in% var_names(.data)) what <- what_name
  active(.data) <- what
  .data
}
#' @name activate
#' @export
activate.hyperfilter <- function(.data, what) {
  what_name <- deparse(substitute(what))
 # if (what_name %in% var_names(.data)) what <- what_name
  active(.data) <- what
  .data
}
#' @param x NetCDF object
#' @name activate
#' @export
active.tidync <- function(x) {
  attr(x, 'active')
}
#' @name activate
#' @export
active.hyperfilter <- active.tidync
#' @param value name of variable to be active
#' @name activate
#' @export
`active<-.tidync` <- function(x, value) {
  vn <- var_names(x)
  if (!value %in% vn) {
    stop(sprintf('Only possible to activate existing variables: %s', paste(vn, collapse = ", ")), call. = FALSE)
  }
  attr(x, 'active') <- value
  x
}
#' @name activate
#' @export
`active<-.hyperfilter` <- function(x, value) {

  attr(x, 'active') <- value
  x
}

#' hyper slab index
#' 
#' @param x tidync object
#' @param ... expressions to `hyper_filter`
#' @param varname variable to be activated
#' @export
hyper_index <- function(x,  ...) {
  UseMethod("hyper_index")
}
#' @export
#' @name hyper_index
hyper_index.tbl_df <- function(x, ...) {
  structure(x, class = c("hyperindex", class(x)))
}
#' @export
#' @name hyper_index
hyper_index.tidync <- function(x, ...) {
  x %>% hyper_filter(...) %>% hyper_index()

}
#' @export
#' @name hyper_index
hyper_index.character <- function(x, varname, ...) {
  out <- tidync(x)
  if (!missing(varname)) out <- activate(out, varname)
  hyper_index(out, ...)
}
#' @export
#' @name hyper_index
#' @importFrom tibble tibble
hyper_index.hyperfilter <- function(x, ...) {
  bind_rows(lapply(x, 
                   function(sub_trans) tibble::tibble(name = sub_trans$name[1], 
                                              start = min(sub_trans$step), 
                                              count = length(sub_trans$step), 
                                              variable = active(x), 
                                              file = sub_trans$filename[1L]))) %>% hyper_index()
}

#' hyper slice
#' 
#' @param x a hyperindex-able thing
#' @param ... ignored
#' @param raw_datavals logical to control whether scaling in the NetCDF is applied or not
#' @export
hyper_slice <- function(x, ..., raw_datavals = FALSE) {
  UseMethod("hyper_slice")
}
#' @name hyper_slice
#' @export
hyper_slice.hyperindex <- function(x, ..., raw_datavals = FALSE) {
  ncdf4::ncvar_get(ncdf4::nc_open(x$file[1]), x$variable[1], 
                   start = x$start, count = x$count, raw_datavals = raw_datavals)
}
#' @name hyper_slice
#' @export
hyper_slice.hyperfilter <- function(x, ..., raw_datavals = FALSE) {
   hyper_index(x, ...) %>% hyper_slice(raw_datavals = raw_datavals)
}
#' @name hyper_slice
#' @export
hyper_slice.tidync <- function(x, ..., raw_datavals = FALSE) {
  x %>% hyper_filter(...) %>% hyper_index() %>% hyper_slice(raw_datavals = raw_datavals)
}
#' @name hyper_slice
#' @export
hyper_slice.character <- function(x, ..., raw_datavals = FALSE) {
  tidync(x) %>% hyper_filter(...) %>%  hyper_index() %>% hyper_slice(raw_datavals = raw_datavals)
}







#' Variable names
#' 
#' 
#' @param x NetCDF object
#' @param ... currently ignored
#'
#' @return names of variables available
#' @export
#'
#' @examples
#' l3file <- "S2008001.L3m_DAY_CHL_chlor_a_9km.nc"
#' rnc <- tidync(system.file("extdata", l3file, package= "ncdump"))
#' var_names(rnc)
var_names <- function(x, ...) {
  UseMethod("var_names")
}
#' @export
#' @name var_names
var_names.character <- function(x, ...) {
  var_names(tidync(x))
}
#' @export
#' @name var_names
var_names.tidync <- function(x, ...) {
  x$variable$name
}

#' Dimension names
#' 
#' 
#' @param x NetCDF object
#' @param ... ignored
#'
#' @return names of available dimensions 
#' @export
#'
#' @examples
#' l3file <- "S2008001.L3m_DAY_CHL_chlor_a_9km.nc"
#' rnc <- tidync(system.file("extdata", l3file, package= "ncdump"))
#' dim_names(rnc)
dim_names <- function(x, ...) {
  UseMethod("dim_names")
}
#' @export
#' @name dim_names
dim_names.character <- function(x, ...) {
  var_names(tidync(x))
}
#' @export
#' @name dim_names
dim_names.tidync <- function(x, ...) {
  x$variable$name
}



#' Dimension values
#' 
#' The dimension values are the coordinates, the positions along each axis. 
#'
#' @param x NetCDF object
#'
#' @return data frame of dimensions and their values
#' @export
#'
#' @examples
#' l3file <- "S2008001.L3m_DAY_CHL_chlor_a_9km.nc"
#' rnc <- tidync(system.file("extdata", l3file, package= "ncdump"))
#' dimension_values(rnc)
dimension_values <- function(x) {
  UseMethod("dimension_values")
}
#' @rdname dimension_values
#' @export
dimension_values.character <- function(x) {
  dimension_values(NetCDF(x))
}
#' @rdname dimension_values
#' @importFrom rlang .data
#' @export
dimension_values.tidync <- function(x) {
  dimids <- x$variable[x$variable$name == active(x), ]
    #dplyr::select(.data$name, .data$.variable_) %>% 
  dimids <- dimids[, c("name", ".variable_")]
  dimids <- dimids %>%  dplyr::inner_join(x$vardim, ".variable_") %>% 
    dplyr::select(.data$.dimension_)
  
  dim_names <- x$dimension[, c("name", ".dimension_")]
  
  ## forcats means we maintain the right order
  dimids %>% #dplyr::transmute(id = dimids) %>%  
    dplyr::inner_join(x$dimension_values, ".dimension_") %>% 
    dplyr::inner_join(dim_names, ".dimension_")
  
  
}

#' Dimensions of a variable
#'
#' @param x NetCDF object
#'
#' @export
variable_dimensions <- function(x) {
  UseMethod("variable_dimensions")
}
#' @rdname variable_dimensions
#' @export
variable_dimensions <- function(x) {
  variable_dimensions(NetCDF(x))
}
#' @rdname variable_dimensions
#' @importFrom dplyr inner_join
#' @export
variable_dimensions <- function(x) {
  #aa <- x$variable %>% 
 aa <- x$variable[x$variable$name == active(x), ]
  #bb <- aa %>% 
  #  dplyr::transmute(variable_name = .data$name, .data$.variable_) 
  bb <- tibble::tibble(variable_name = aa$name, .variable_ = aa$.variable_)
  cc <- bb %>% 
    dplyr::inner_join(x$vardim, ".variable_") %>% 
    dplyr::inner_join(x$dimension, ".dimension_")
  dd <- tibble::tibble(variable_name = cc$variable_name, 
               .variable_ = cc$.variable_, 
               .dimension_ = cc$.dimension_, 
               dimension_name = cc$name)
dd
}





#' Print NetCDF object
#' 
#' print S3 method
#' 
#' Prints a summary of variables and dimensions. 
#' @param x NetCDF object
#'
#' @param ... reserved
#'
#' @rdname print-NetCDF
#' @export
#' @importFrom dplyr %>% arrange transmute
print.tidync <- function(x, ...) {
  form <- active(x)
  vn <- c(form, setdiff(var_names(x), form))
  if (length(vn)> 1) {
    form <- sprintf("%s, (%s)", vn[1L],  paste(vn[-1L], collapse = ", "))
  }
  cat(sprintf("Variables: %s", form), "\n")
  
  cat(sprintf("Dimensions: \n", ""))
  print(variable_dimensions(x) %>% 
          inner_join(x$dimension %>% dplyr::transmute(.data$.dimension_, dimension_length = .data$len), ".dimension_"))
  # print(x$dimension %>% 
  #         dplyr::arrange(.data$id)  %>% 
  #         transmute(.data$name, length = .data$len, 
  #                   unlimited= .data$unlim ) %>% as.data.frame())
  invisible(NULL)
}

