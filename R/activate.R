
#' Activate
#'
#' Set the context for subsequent manipulations. 
#' 
#' `activate` puts the named variable first
#' `active` gets and sets the active variable
#' @param .data NetCDF object
#' @param what name of a variable
#' @param ... reserved, currently ignored
#' @param select_var optional argument to set selected state of variable/s by name
#' @return NetCDF object
#' @export active activate active<- 
#' @rdname activate 
#' @aliases active activate active<- 
#' @examples
#' l3file <- "S20092742009304.L3m_MO_CHL_chlor_a_9km.nc"
#' rnc <- tidync(system.file("extdata", "oceandata", l3file, package = "tidync"))
#' activate(rnc, "palette")
#' @name activate
#' @export
activate <- function(.data, what, ..., select_var = NULL) UseMethod("activate")
#' @name activate
#' @export
activate.tidync <- function(.data, what, ..., select_var = NULL) {
  if (missing(what)) return(.data)
  what_name <- deparse(substitute(what))
  #print(what_name)
  #if (what_name %in% var_names(.data)) what <- what_name
  if (what_name %in% .data$grid$variable) {
    ## use the variable to find the grid
    what <- .data$grid$grid[.data$grid$variable == what_name]
    select_var <- what_name
  } else if (what %in% .data$grid$variable){
    select_var <- what
    what <- .data$grid$grid[.data$grid$variable == what]
    
  }

  if (is.numeric(what)) {
    ## this pattern is copied from print
    ushapes <- dplyr::distinct(.data$grid) %>% 
      dplyr::arrange(desc(nchar(.data$grid)))
    ## otherwise pick the what-th grid
    stopifnot(what >= 1 && what <= nrow(.data$grid))
    what <- ushapes$grid[as.integer(what)]
  
  }
  active(.data) <- what

  active_variables <- .data[["grid"]] %>% dplyr::filter(.data$grid == what) %>% 
    dplyr::inner_join(.data[["variable"]], c("variable" = "name"))
  if (!is.null(select_var)) {
    active_variables <- inner_join(active_variables, tibble::tibble(variable = select_var))
  }
  active_dimensions <- as.integer(gsub("^D", "", unlist(strsplit(active(.data), ","))))
  .data$dimension$active <- rep(FALSE, nrow(.data$dimension))
  .data$dimension$active[active_dimensions + 1] <- TRUE
  .data[["variable"]] <-  mutate(.data[["variable"]], active = .data$name %in% active_variables$variable)
  .data <- update_slices(.data)
  .data
}
#' @param x NetCDF object
#' @name activate
#' @export
active.tidync <- function(x) {
  attr(x, 'active')
}
`active<-.tidync` <- function(x, value) {
  #vn <- var_names(x)
  sn <- unique(x$grid$grid)
  if (!value %in% sn) {
    #stop(sprintf('Only possible to activate existing variables: %s', paste(vn, collapse = ", ")), call. = FALSE)
     
   stop(sprintf('Only possible to activate grids by name (or number, or by nominated variable): \n%s', paste(sn, collapse = "\n")), call. = FALSE)
  }
  attr(x, 'active') <- value
  x
}

#' @export
activate.default <- function(.data, what, ..., select_var = NULL) {
  what_name <- deparse(substitute(what))
  if (what_name %in% names(.data)) what <- what_name
  active(.data) <- what
  .data
}

#' @rdname activate
#' @export
active <- function(x) {
  UseMethod("active")
}
#' @rdname activate
#' @export
active.default <- function(x) {
  warning("determining active status of object not recognized as activatable")
  val <- attr(x, 'active')
}
#' @rdname activate
#' @param value name of grid or variable to be active
#' @export
`active<-` <- function(x, value) {
  UseMethod("active<-")
}
#' @rdname activate
#' @export
`active<-.default` <- function(x, value) {
  warning("activating as a default, this object not recognized as activatable")
  attr(x, 'active') <- value
  x
}
