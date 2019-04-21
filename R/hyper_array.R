#' Extract NetCDF data as an array
#'
#' Extract the raw array data as a list of  one or more arrays. This can be the
#' entire variable/s or after dimension-slicing using [hyper_filter()]
#' expressions. This is a delay-breaking function and causes data to be read 
#' from the source into raw arrays in R's native form. This list of arrays is 
#' lightly classed as [tidync_data], with methods for [print()] and [tidync()]. 
#'
#' The function [hyper_array()] will act on an existing tidync object or a source
#' string.
#'
#' By default all variables in the active grid are returned, use `select_var` to
#' specify one or more desired variables.
#'
#' The transforms are stored as a list of tables in an attribute `transforms``,
#' access these with [hyper_transforms()].
#' @param x NetCDF file, connection object, or [tidync] object
#' @param drop collapse degenerate dimensions, defaults to `TRUE`
#' @param ... passed to [hyper_filter()]
#' @param select_var optional vector of variable names to select
#' @param raw_datavals logical to control whether scaling in the NetCDF is
#'   applied or not
#' @param force ignore caveats about large extraction and just do it
#'
#' @export
#' @aliases tidync_data
#' @seealso [print.tidync_data] for a description of the print summary, 
#' [hyper_tbl_cube()] and [hyper_tibble()] which are also delay-breaking 
#' functions that cause data to be read 
#' @examples
#' f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#'
#' ## extract a raw list by filtered dimension
#' library(dplyr)
#' araw1 <- tidync(l3file) %>%
#'  hyper_filter(lat = between(lat, -78, -75.8), 
#'               lon = between(lon, 165, 171)) %>%
#'  hyper_array()
#'
#' araw <- tidync(l3file) %>% 
#'          hyper_filter(lat = abs(lat) < 10, 
#'                      lon = index < 100) %>%
#'   hyper_array()
#'
#' ## hyper_array will pass the expressions to hyper_filter
#' braw <- tidync(l3file) %>% 
#'   hyper_array(lat = abs(lat) < 10, lon = index < 100)
#'
#' ## get the transforms tables (the axis coordinates)
#' lapply(attr(braw, "transforms"), 
#'    function(x) nrow(dplyr::filter(x, selected)))
#' ## the selected axis coordinates should match in order and in size
#' lapply(braw, dim)
hyper_array <- function(x, select_var = NULL, ..., 
                        raw_datavals = FALSE, force = FALSE, drop = TRUE) {
  UseMethod("hyper_array")
}

## alias
#' @name hyper_array
#' @export
hyper_slice <- function(x, select_var = NULL, ..., 
                        raw_datavals = FALSE, force = FALSE, drop = TRUE) {
  warning("hyper_array should be used instead of hyper_slice")
  hyper_array(x = x, select_var = select_var, ..., 
              raw_datavals = raw_datavals, force = force, drop = drop)
}
#' @name hyper_array
#' @export
hyper_array.tidync <- function(x, select_var = NULL, ..., 
                          raw_datavals = FALSE, force = FALSE, drop = TRUE) {
  x <- hyper_filter(x, ...) 
  variable <- x[["variable"]] %>% dplyr::filter(active)
  varname <- unique(variable[["name"]])
  ## hack to get the order of the indices of the dimension
  ordhack <- 1 + as.integer(unlist(strsplit(gsub("D", "", 
                          dplyr::filter(x$grid, .data$grid == active(x)) %>% 
                                 dplyr::slice(1L) %>% 
                                 dplyr::pull(.data$grid)), ",")))
  dimension <- x[["dimension"]] %>% dplyr::slice(ordhack)
  ## ensure dimension is in order of the dims in these vars
  axis <- x[["axis"]] %>% dplyr::filter(variable %in% varname)
  ## dimension order must be same as axis
  START <- dimension$start
  COUNT <- dimension$count
 # browser()
  if (is.null(select_var))   {
    varnames <- varname
  } else {
    if (!all(select_var %in% variable[["name"]])) {
      bad <- base::setdiff(select_var, variable[["name"]])
      
      select_var <- base::intersect(select_var, variable[["name"]])
      if (length(select_var) < 1) stop("no select_var variables available")
      warning(sprintf("some select_var variables not found, and ignored:\n %s",
                      paste(bad, collapse = ",")))
    }
    ## todo, make this quosic?
    varnames <- select_var
  }

  ## naughty  internal function using scope for 
  ##   x, START, COUNT, con, raw_datavals, drop
  get_vara <- function(vara)  {
    con <- ncdf4::nc_open(x$source$source[1])
    on.exit(ncdf4::nc_close(con), add =   TRUE)
    ncdf4::ncvar_get(con, vara, 
                     start = START, count = COUNT, 
                     raw_datavals = raw_datavals, collapse_degen = drop)
  }
  mess <- sprintf("pretty big extraction here with (%i*%i values [%s]*%i", 
                  as.integer(prod( COUNT)), length(varnames), 
                  paste( COUNT, collapse = ", "), 
                  length(varnames))
  if ((prod(dimension[["count"]]) * length(varnames)) > 1.2e9 && 
        interactive() && !force) {
    yes <- yesno::yesno(mess, "\nProceed?")
    if (!yes) return(invisible(NULL))
  }
  transforms <- x[["transforms"]][x[["dimension"]] %>% 
                                    dplyr::filter(.data$active) %>% 
                                    dplyr::pull(.data$name)]
  datalist <- lapply(varnames, get_vara)
  
  
  ## which of the variables for read are NC_CHAR? (they have to be split)
  charvars <- variable$type[match(varnames, variable$name)] == "NC_CHAR"
  if (any(charvars)) {
    idx <- which(charvars)
    for (i in seq_along(idx)) {
      
      ii <- idx[i]
      datalist[[ii]] <- array(unlist(strsplit(datalist[[ii]], "")), 
                              dimension$count)
    }
  }
  structure(datalist, names = varnames, 
            transforms = transforms, 
            source = x$source, class = "tidync_data")
}
#' @name hyper_array
#' @export
hyper_array.character <- function(x,  select_var = NULL, ...,
                                  raw_datavals = FALSE, drop = TRUE) {
  tidync(x) %>% 
    hyper_filter(...) %>%  
    hyper_array(select_var = select_var, 
                raw_datavals = raw_datavals, drop = drop)
}
