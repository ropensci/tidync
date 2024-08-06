#' @keywords internal
"_PACKAGE"

#' Tidy tools for NetCDF data.
#'
#' Provides easy to use idioms for working with NetCDF data for extraction,
#' manipulation and visualization. NetCDF is Network Common Data Form
#' \url{https://www.unidata.ucar.edu/software/netcdf/}.
#' 
#' See [print.tidync()] for details on the printed version of a tidync object. 
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
#'  \code{\link{hyper_grids}} \tab information on grids \cr
#'  }
#'  The scheme generally processes dimension filters into NetCDF extraction
#'  indexes and these are always available to each function, and are expressed
#'  in printed output.
#'  
#' The following options are available. 
#' \tabular{ll}{
#' \code{tidync.large.data.check = TRUE/FALSE} \tab check for large data extraction (default `TRUE`) \cr
#' \code{tidync.silent = FALSE/TRUE} \tab emit warnings,messages or be silent (default `FALSE`) \cr
#' }
#' @name tidync-package
#' @examples 
#' argofile <- system.file("extdata/argo/MD5903593_001.nc", package = "tidync")
#' argo <- tidync(argofile)
#' argo %>% active()
#' argo %>% activate("D3,D8") %>% hyper_array()
#' argo %>% hyper_filter(N_LEVELS = index < 4)
#' argo %>% hyper_tbl_cube()
#' argo %>% hyper_tibble(select_var = c("TEMP_QC"))
#' argo %>% hyper_transforms()
#' argo %>% hyper_vars()
#' argo %>% hyper_dims()
#' argo %>% hyper_grids()
#' 
#' ## some global options
#' getOption("tidync.large.data.check")
#' 
#' getOption("tidync.silent")
#' op <- options(tidync.silent = TRUE)
#' getOption("tidync.silent")
#' options(op)
NULL
