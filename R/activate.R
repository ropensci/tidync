
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
  if (is.null(what)) return(.data)
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
  #vn <- var_names(x)
  sn <- unique(shapes(x)$shape)
  if (!value %in% sn) {
    #stop(sprintf('Only possible to activate existing variables: %s', paste(vn, collapse = ", ")), call. = FALSE)
     
   stop(sprintf('Only possible to activate shapes: %s', paste(sn, collapse = ", ")), call. = FALSE)
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
