#' hyper slab index
#' 
#' @param x tidync object
#' @param ... expressions to `hyper_filter`
#' @param varname variable to be activated
#' @export
hyper_index <- function(x,  ...) {
  UseMethod("hyper_index")
}
#' @export
#' @name hyper_index
hyper_index.tbl_df <- function(x, ...) {
  structure(x, class = c("hyperindex", class(x)))
}
#' @export
#' @name hyper_index
hyper_index.tidync <- function(x, ...) {
  x %>% hyper_filter(...) %>% hyper_index()
  
}
#' @export
#' @name hyper_index
hyper_index.character <- function(x, varname, ...) {
  out <- tidync(x)
  if (!missing(varname)) out <- activate(out, varname)
  hyper_index(out, ...)
}
#' @export
#' @name hyper_index
#' @importFrom tibble tibble
hyper_index.hyperfilter <- function(x, ...) {
  out <- bind_rows(lapply(x, 
                   function(sub_trans) tibble::tibble(name = sub_trans$name[1], 
                                                      start = min(sub_trans$index), 
                                                      count = length(sub_trans$index), 
                                                      grid = active(x), 
                                                      file =  attr(x, "source")$source[1])))
  ## FIXME: hack we shouldn't need https://github.com/hypertidy/tidync/issues/33
  out$variable <- lapply(seq_len(nrow(out)), function(a) variables_from_grid(attr(x, "grid"), active(x)))

                hyper_index(out)
}
variables_from_grid <- function(grids, agrid) {
  dplyr::filter(grids, grid == agrid)$variable
}