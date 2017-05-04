
#' Variable names
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
varnames <- function(x, ...) {
  UseMethod("varnames")
}
#' @export
#' @name varnames
varnames.character <- function(x, ...) {
  varnames(NetCDF(x))
}
#' @export
#' @name varnames
varnames.NetCDF <- function(x, ...) {
  x$variable$name
}

#' Activate variable
#'
#' @param .data 
#' @param what 
#'
#' @return
#' @export
#'
#' @examples
nctivate <- function(.data, what) {
  UseMethod('nctivate')
}
#' @export
#' @rdname nctivate
nctivate.NetCDF <- function(.data, what) {
  what_name <- deparse(substitute(what))
  if (what_name %in% varnames(.data)) what <- what_name
  nctive(.data) <- what
  .data
}
#' @rdname nctivate
#' @export
nctive <- function(x) {
  attr(x, 'nctive')
}
`nctive<-` <- function(x, value) {
  vn <- varnames(x)
  if (!value %in% vn) {
    stop(sprintf('Only possible to activate existing variables: %s', paste(vn, collapse = ", ")), call. = FALSE)
  }
  attr(x, 'nctive') <- value
  x
}

#' Dimension values
#' 
#' The dimension values are the coordinates, the positions along each axis. 
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
dimension_values <- function(x) {
  UseMethod("dimension_values")
}
#' @rdname dimension_values
#' @export
dimension_values.NetCDF <- function(x) {
  dimids <- x$variable %>% filter(name == nctive(x)) %>% select(name, id) %>% inner_join(x$vardim) %>% select(dimids)
  ## forcats means we maintain the right order
  dimids %>% transmute(id = dimids) %>%  inner_join(x$dimvals) %>% inner_join(x$dimension %>% select(id, name))  ##%>% split(forcats::as_factor(.$name))
}
filter.NetCDF <- function(x, ...) {
  ## rlang::quos(...)
  dimvals <- dimension_values(x)
  dimvals %>% split(forcats::as_factor(.$name)) %>% purrr::map(.f = )
}
#' @rdname nctivate
#' @export
#' @examples 
#' f <- "/rdsi/PRIVATE/raad/data/eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/2017/AVHRR/avhrr-only-v2.20170502_preliminary.nc"
#' x <- NetCDF(f)
#' nctive(x)
#' x <- nctivate(x, "sst") 
print.NetCDF <- function(x) {
  activ <- nctive(x)
  print(sprintf("Active: %s", activ))
  vn <- setdiff(varnames(x), activ)
  if (length(vn)> 0) {
  print(sprintf("(%s)", paste(vn, collapse = ", ")))
  }
}

