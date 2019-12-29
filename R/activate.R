
#' Activate a NetCDF grid
#'
#' A grid in NetCDF is a particular shape and size available for array
#' variables, and consists of sets of dimensions. To activate a grid is to set
#' the context for downstream operations, for querying, summarizing and reading
#' data. There's no sense in performing these operations on more than one grid
#' at a time, but multiple variables may exist in a single grid. There may be
#' only one significant grid in a source or many, individual dimensions are
#' themselves grids.
#'
#' There may be more than one grid and one is always activated by default. A
#' grid may be activated by name in the form of 'D1,D0' where one or more
#' numbered dimensions indicates the grid. The grid definition names are printed
#' as part of the summary of in the tidync object and may be obtained directly
#' with [hyper_grids()] on the tidync object.
#'
#' Activation of a grid sets the context for downstream operations (slicing and
#' reading data) from NetCDF, and as there may be several grids in a single
#' source activation allows a different choice of available variables.  By
#' default the largest grid is activated. Once activated, all downstream tasks
#' apply to the set of variables that exist on that grid.
#'
#' If [activate()] is called with a variable name, it puts the variable first.
#' The function [active()] gets and sets the active grid. To restrict ultimate
#' read to particular variables use the `select_var` argument to
#' [hyper_filter()], [hyper_tibble()] and [hyper_tbl_cube()].
#'
#' Scalar variables are not currently available to tidync, and it's not obvious
#' how activation would occur for scalars, but in future perhaps `activate("S")`
#' could be the right way forward.
#' @param .data NetCDF object
#' @param what name of a grid or variable
#' @param ... reserved, currently ignored
#' @param select_var optional argument to set selected state of variable/s by
#'   name
#' @return NetCDF object
#' @export active activate active<-
#' @rdname activate
#' @aliases active activate active<-
#' @examples
#' if (!tolower(Sys.info()[["sysname"]]) == "sunos") {
#'  l3file <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#'  rnc <- tidync(system.file("extdata", "oceandata", l3file,
#'  package = "tidync"))
#'  activate(rnc, "palette")
#'
#'  ## extract available grid names
#'  hyper_grids(rnc)
#' }
#' @seealso hyper_filter hyper_tibble hyper_tbl_cube
#' @name activate
#' @export
activate <- function(.data, what, ..., select_var = NULL) UseMethod("activate")
#' @name activate
#' @export
activate.tidync <- function(.data, what, ..., select_var = NULL) {
  if (missing(what)) return(.data)
  if (utils::packageVersion("tidyr") > "0.8.3" ) {
   vargrids <- tidyr::unnest(.data$grid, cols = c(.data$variables)) 
  } else {
   vargrids <- tidyr::unnest(.data$grid) 
  }
  what_name <- deparse(substitute(what))
  #if (what_name %in% var_names(.data)) what <- what_name
  if (what_name %in% vargrids$variable) {
    ## use the variable to find the grid
    what <- vargrids$grid[vargrids$variable == what_name]
    select_var <- what_name
  } else if (what %in% .data$variable$name){
    if (!is.null(select_var)) {
      vargrids <- vargrids[vargrids$variable == select_var, , drop = FALSE]
    } else {
      vargrids <- vargrids[vargrids$variable == what[1], , drop = FALSE]
    }
    
    what <- vargrids$grid[1L]
  }

  if (is.numeric(what)) {
    stop("numeric 'what' is not supported, use grid name or variable name")
    ## this pattern is copied from print
    ## remove $variables because it a list 
    ushapes <- dplyr::distinct(.data$grid %>% 
                                 dplyr::select(-.data$variables)) %>% 
      dplyr::arrange(desc(nchar(.data$grid)))
    ## otherwise pick the what-th grid
    stopifnot(what >= 1 && what <= nrow(.data$grid))
    what <- ushapes$grid[as.integer(what)]
  
  }
  active(.data) <- what

  active_variables <- vargrids %>% dplyr::filter(.data$grid == what) %>% 
    dplyr::inner_join(.data[["variable"]], c("variable" = "name"))

  if (!is.null(select_var)) {
    active_variables <- inner_join(active_variables, 
                                   tibble::tibble(variable = select_var), 
                                   "variable")
  }
  active_dimensions <- as.integer(gsub("^D", "", 
                                       unlist(strsplit(active(.data), ","))))
  .data$dimension$active <- rep(FALSE, nrow(.data$dimension))
  .data$dimension$active[active_dimensions + 1] <- TRUE
  .data[["variable"]] <-  
    mutate(.data[["variable"]], 
    active = .data$name %in% active_variables$variable)
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
    txt <- 'Activate grids by name (or number, or by nominated variable): \n%s'
    mess <- sprintf(txt, 
                    paste(sn, collapse = "\n"))
   stop(mess, call. = FALSE)
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
