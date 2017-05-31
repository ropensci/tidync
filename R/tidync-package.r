#' Tidy tools for NetCDF data.
#'
#' Provides easy to use idioms for working with NetCDF data for
#' extraction, manipulation and visualization. 
#' NetCDF is Network Common Data Form \url{https://www.unidata.ucar.edu/software/netcdf/}. 
#' 
#' There is a family of functions "hyper_verb" around exploring and
#' extracting data.  
#' \tabular{ll}{
#'  \code{\link{tidync}} \tab extracts all file metadata in an object useable by other tidync functions\cr
#'  \code{\link{hyper_filter}} \tab processes user-driven expressions to filter based on NetCDF dimensions \cr
#'  \code{\link{hyper_index}} \tab processes dimension filters into NetCDF extraction indexes \cr
#'  \code{\link{hyper_slice}} \tab extracts a raw data array based on a NetCDF index \cr
#'  \code{\link{hyper_tibble}} \tab extracts the slice as a data frame with all dimension values \cr
#'  }
#' @name tidync-package
#' @docType package
NULL
