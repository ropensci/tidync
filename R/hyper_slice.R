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
  ## FIXME: the change to grid means we get x$grid[1] not variable, we have
  ## to slice over all variables in this space
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

