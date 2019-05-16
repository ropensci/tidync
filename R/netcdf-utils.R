#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom RNetCDF open.nc close.nc var.get.nc
#' @importFrom purrr safely
nc_get <- function(x, v, test = FALSE, ...) {
  UseMethod("nc_get")
}
nc_get.character <- function(x, v, test = FALSE, ...) {
  if (!test) {
    con <- RNetCDF::open.nc(x)
    on.exit(RNetCDF::close.nc(con), add = TRUE)
  
    safe_get <- purrr::safely(nc_get.NetCDF)
    val <- safe_get(con, v)
    if (!is.null(val$result)) return(val$result)
  } else {
  con4 <- NULL
  con4 <- ncdf4::nc_open(x, readunlim = FALSE, verbose = FALSE, 
                         auto_GMT = FALSE, suppress_dimvals = TRUE)
  on.exit(ncdf4::nc_close(con4), add = TRUE)
  safe_get4 <- purrr::safely(nc_get.ncdf4)
  val <- safe_get4(con4, v)
  if (is.null(val[["result"]])) {
    stop(sprintf("no variable found %s", v))
  } else {
    return(val[["result"]])
  }
  
  }
  stop(sprintf("no variable found %s", v))
}
nc_get.NetCDF <- function(x, v) {
  RNetCDF::var.get.nc(x, v)
}


nc_get.ncdf4 <- function(x, v) {
  ncdf4::ncvar_get(x, v)
}
