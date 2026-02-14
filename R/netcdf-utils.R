#' Helper to get a variable from NetCDF. 
#' 
#' This exists so we can (internally) use a file path, uri, or open
#' NetCDF connection (ncdf4 or RNetCDF) in a simpler way. 
#' 
#' This function just reads the whole array. It is equivalent to the 
#' angstroms package function 'rawdata(x, varname)'. 
#' @param x file path, uri, or NetCDF connection
#'
#' @param v variable name
#' @param test if true we make sure the connection can be open, not applied for connections themselves
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom RNetCDF open.nc close.nc var.get.nc
#' @export
nc_get <- function(x, v, test = FALSE) {
  UseMethod("nc_get")
}
#' @export
nc_get.character <- function(x, v, test = FALSE) {
  if (!test) {
    con <- RNetCDF::open.nc(x)
    on.exit(RNetCDF::close.nc(con), add = TRUE)

    val <- tryCatch(nc_get.NetCDF(con, v), error = function(e) NULL)
    if (!is.null(val)) return(val)
  } else {
  con4 <- NULL
  ## issue #119
  suppressWarnings(
  con4 <- ncdf4::nc_open(x, readunlim = FALSE, verbose = FALSE, 
                         auto_GMT = FALSE, suppress_dimvals = TRUE))
  on.exit(ncdf4::nc_close(con4), add = TRUE)
  val <- tryCatch(nc_get.ncdf4(con4, v), error = function(e) NULL)
  if (is.null(val)) {
    stop(sprintf("no variable found %s", v))
  } else {
    return(val)
  }
  
  }
  stop(sprintf("no variable found %s", v))
}

#' @export
nc_get.NetCDF <- function(x, v, test = FALSE) {
  RNetCDF::var.get.nc(x, v)
}

#' @export
nc_get.ncdf4 <- function(x, v, test = FALSE) {
  ncdf4::ncvar_get(x, v)
}

#' Read the length of a single dimension from a NetCDF source.
#'
#' Used internally by the multi-source constructor in fast mode when
#' the concat dimension has no coordinate variable (coord_dim = FALSE).
#'
#' @param x file path or URI
#' @param dimname name of the dimension
#' @return integer, the dimension length
#' @noRd
nc_dim_len <- function(x, dimname) {
  con <- RNetCDF::open.nc(x)
  on.exit(RNetCDF::close.nc(con), add = TRUE)
  info <- RNetCDF::dim.inq.nc(con, dimname)
  info$length
}
