#' Tidy tools for NetCDF data.
#'
#' Provides easy to use idioms for working with NetCDF data for extraction,
#' manipulation and visualization. NetCDF is Network Common Data Form
#' \url{https://www.unidata.ucar.edu/software/netcdf/}.
#' 
#' There is a family of functions "hyper_verb" around exploring and
#' extracting data.  
#' \tabular{ll}{
#'  \code{\link{active}} \tab report the currently active grid \cr
#'  \code{\link{activate}} \tab active a grid \cr
#'  \code{\link{tidync}} \tab core NetCDF source object for tidync functions\cr
#'  \code{\link{hyper_filter}} \tab apply dimension expressions to specify array slices\cr
#'  \code{\link{hyper_array}} \tab extracts a raw data array based on a NetCDF index \cr
#'  \code{\link{hyper_tbl_cube}} \tab extracts data as a dplyr tbl_cube \cr
#'  \code{\link{hyper_tibble}} \tab extracts data as a data frame with all dimension values\cr
#'  \code{\link{hyper_transforms}} \tab extract the active (or all) dimension transforms\cr
#'  \code{\link{hyper_vars}} \tab information on active variables \cr
#'  \code{\link{hyper_dims}} \tab information on active dimensions \cr
#'  }
#'  The scheme generally processes dimension filters into NetCDF extraction
#'  indexes and these are always available to each function, and are expressed
#'  in print methods.
#' @name tidync-package
#' @docType package
NULL