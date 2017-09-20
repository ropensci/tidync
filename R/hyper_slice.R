#' hyper slice
#' 
#' By default all variables in the active grid are returned, use `select_var` to limit. 
#' @param x tidync object
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
hyper_slice.tidync <- function(x, select_var = NULL, ..., raw_datavals = FALSE) {
  variable <- x[["variable"]] %>% dplyr::filter(active)
  varname <- unique(variable[["name"]])
  dimension <- x[["dimension"]] %>% dplyr::filter(active)
  axis <- x[["axis"]] %>% dplyr::filter(variable == varname)
  ## dimension order must be same as axis
  START <- dimension$start[match(axis$dimension, dimension$id)]
  COUNT <- dimension$count[match(axis$dimension, dimension$id)]
  if (is.null(select_var))   {
    varnames <- variable %>% dplyr::pull(name)
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
    con <- ncdf4::nc_open(x$source$source[1])
    on.exit(ncdf4::nc_close(con), add =   TRUE)
    ncdf4::ncvar_get(con, vara, 
                     start = START, count = COUNT, 
                     raw_datavals = raw_datavals)
  }
  mess <- sprintf("pretty big extraction here with (%i*%i values [%s]*%i", 
                  as.integer(prod( COUNT)), length(varnames), 
                  paste( COUNT, collapse = ", "), 
                  length(varnames))
  #if (prod(x$count) > 1e8) warning(mess)
  if ((prod(dimension[["count"]]) * length(varnames)) > 1.2e9 & interactive()) {
    yes <- yesno::yesno(mess, "\nProceed?")
    if (!yes) return(invisible(NULL))
  }
  
  stats::setNames(lapply(varnames, get_vara), varnames)
}
#' @name hyper_slice
#' @export
hyper_slice.character <- function(x, select_var = NULL, ..., raw_datavals = FALSE) {
  tidync(x) %>% hyper_filter(...) %>%  hyper_slice(select_var = select_var, raw_datavals = raw_datavals)
}
