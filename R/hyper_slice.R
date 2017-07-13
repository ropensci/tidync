#' hyper slice
#' 
#' By default all variables in the active grid are returned, use `select_var` to limit. 
#' @param x a hyperindex-able thing
#' @param ... ignored
#' @param select_var optional vector of variable names to select
#' @param raw_datavals logical to control whether scaling in the NetCDF is applied or not
#'
#' @export
hyper_slice <- function(x, select_var = NULL, ..., raw_datavals = FALSE) {
  UseMethod("hyper_slice")
}
#' @name hyper_slice
#' @export
hyper_slice.hyperindex <- function(x, select_var = NULL, ..., raw_datavals = FALSE) {
  if (is.null(select_var))   {
    varnames <- x$variable[[1]]
  } else {
    if (!any(select_var %in% x$variable[[1]])) {
      print("available variables: ")
      print(paste(x$variable[[1]], collapse = ", "))
      stop(sprintf("some select_var variables not found %s", select_var))
    }
    ## todo, make this quosic?
    varnames <- select_var
  }
  get_vara <- function(vara)  {
    con <- ncdf4::nc_open(x$file[1])
    on.exit(ncdf4::nc_close(con), add =   TRUE)
    ncdf4::ncvar_get(con, vara, 
                     start = x$start, count = x$count, 
                     raw_datavals = raw_datavals)
  }
  mess <- sprintf("pretty big extraction here with (%i*%i values [%s]*%i", 
                  prod(x$count), length(varnames), 
                  paste(x$count, collapse = ", "), 
                  length(varnames))
  #if (prod(x$count) > 1e8) warning(mess)
  if ((prod(x$count) * length(varnames)) > 1.2e8 & interactive()) {
    yes <- yesno::yesno(mess, "\nProceed?")
    if (!yes) return(invisible(NULL))
  }

  stats::setNames(lapply(varnames, get_vara), varnames)
 }
#' @name hyper_slice
#' @export
hyper_slice.hyperfilter <- function(x, select_var = NULL, ..., raw_datavals = FALSE) {
  hyper_index(x, ...) %>% hyper_slice(select_var = select_var, raw_datavals = raw_datavals)
}
#' @name hyper_slice
#' @export
hyper_slice.tidync <- function(x, select_var = NULL, ..., raw_datavals = FALSE) {
  x %>% hyper_filter(...) %>% hyper_index() %>% hyper_slice(select_var = select_var, raw_datavals = raw_datavals)
}
#' @name hyper_slice
#' @export
hyper_slice.character <- function(x, select_var = NULL, ..., raw_datavals = FALSE) {
  tidync(x) %>% hyper_filter(...) %>%  hyper_index() %>% hyper_slice(select_var = select_var, raw_datavals = raw_datavals)
}


