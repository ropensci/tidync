#' dplyr tbl cube from NetCDF
#'
#' @param x filename
#' @param ... arguments for `hyper_filter`
#'
#' @return
#' @export
#'
#' @examples
hyper_tbl_cube <- function(x, ...) {
  UseMethod("hyper_tbl_cube")
}
#' @name hyper_tbl_cube
#' @export
#' @importFrom stats setNames
hyper_tbl_cube.tidync <- function(x, ...) {
  x %>% hyper_filter(...) %>% hyper_tbl_cube()
}
#' @name hyper_tbl_cube
#' @export
hyper_tbl_cube.character <- function(x, ...) {
  stop("direct file access not yet supported, please use tidync(file) %>% hyper_tbl_cube(...)")
} 
#' @name hyper_tbl_cube
#' @export
hyper_tbl_cube.hyperfilter <- function(x, ...) {
  varname <- active(x)
 # hf <- hyper_filter(x, ...)
  dim_names <- names(x)
  structure(list(mets = stats::setNames(list(hyper_slice(x, ...)), varname), 
            dims = stats::setNames(  lapply(dim_names, function(inm) x[[inm]][[inm]]), 
                              dim_names)), 
            class = "tbl_cube")
}
