#' Grid status
#'
#' Functions to report on the current status of the `active` grid. Information
#' on the active dimensions and variables are listed in a data frame with
#' multiple columns.
#'
#' The dimensions and variables of the active grid are identified in the
#' [print][print.tidync()] method of the tidync object, these functions exist to
#' provide that information directly.
#'
#' `hyper_vars()` will list the ids, data type, name, dimension number, number
#' of attributes and and coordinate status of the variables on the currently
#' active grid.
#' 
#' `hyper_dims()` will list the names, lengths, start/count index, ids, and
#' status of dimensions on the currently active grid. records on the currently
#' active dimensions. 
#' 
#' `hyper_grids()` will list the names, number of dimension, and number of 
#' variables and active status of each grid in the source.
#' @param x tidync object
#' @param ... ignored
#'
#' @return data frame
#' @export
#' @aliases hyper_dims hyper_grids
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
  act <- hyper_transforms(x)
  out <- tibble::tibble(name = names(act), 
         length = unlist(lapply(act, nrow)), 
         start = unlist(lapply(act, function(a) which(a$selected)[1])), 
         count  = unlist(lapply(act, function(a) sum(a$selected))))
  act1 <- x$dimension %>% dplyr::filter(.data$active)
  act1[c("active", "start", "count")] <- NULL
  dplyr::inner_join(out, act1, c("length", "name"))
}

#' @name hyper_vars
#' @export
hyper_grids <- function(x, ...) {
  x$grid[c("grid", "ndims", "nvars")] %>% 
    mutate(active = .data$grid == active(x))
}
