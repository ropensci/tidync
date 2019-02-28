# tidync         ## open the file
# activate       ## change the active grid
# hyper_filter   ## apply subsetting by slices on the active grid's axes
# hyper_group_by ## apply classification nominated space (*2) by polygons
# hyper_slice    ## apply the previous setup to pull the arrays
# hyper_tibble   ## arrange output as a table


## utility functions
## names of variables on grid, including lower dims in the right order
## return start and counts, min/max for dims in the active grid 
## return ditto for filtered versions
## utility to burn polygons into a NetCDF of nominated mask space

#' Hyper utilities
#'
#' Functions to report on the current status of the `active` grid. Information on the active
#' dimensions and variables are listed in a data frame with multiple columns.
#'
#' The dimensions and variables of the active grid are identified in the
#' [print][print.tidync()] method of the tidync object, these functions exist to
#' provide that information programmatically.
#'
#' `hyper_vars()` will list the ids, data type, name, dimension number, number of attributes and and coordinate status of the variables on the currently active
#' grid. 
#' 
#' `hyper_dims()` will list the names, lengths, start/count index, ids, and status of dimensions on the currently active grid. 
#' records on the currently active dimensions
#' @param x tidync object
#' @param ... ignored
#'
#' @return data frame
#' @export
#'
#' @examples
#' f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#' tnc <- tidync(l3file)
#' hyper_vars(tnc)
#' hyper_dims(tnc)
#' hyper_dims(tnc %>% hyper_filter(lat = lat < 20))
hyper_vars <- function(x, ...) {
  out <- x[["variable"]] %>% dplyr::filter(.data$active) 
  out[["active"]] <- NULL
  out
}

#' @name hyper_vars
#' @export
hyper_dims <- function(x, ...) {
  act <- active_axis_transforms(x)
  out <- tibble::tibble(name = names(act), 
         length = unlist(lapply(act, nrow)), 
         start = unlist(lapply(act, function(a) which(a$selected)[1])), 
         count  = unlist(lapply(act, function(a) sum(a$selected))))
  act1 <- x$dimension %>% dplyr::filter(.data$active)
  act1[c("active", "start", "count")] <- NULL
  dplyr::inner_join(out, act1, c("length", "name"))
}

