#' dplyr tbl cube 
#' 
#' Produce a dplyr `tbl_cube` from NetCDF. 
#' 
#' #'
#' @param x tidync object
#' @param ... arguments for `hyper_filter`
#'
#' @return tbl_cube
#' @export
#'
#' @examples
#' f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#' (cube <- hyper_tbl_cube(tidync(l3file) %>%
#' activate(chlor_a), lon = lon > 107, lat = abs(lat) < 30))
#' ufile <- system.file("extdata", "unidata", "test_hgroups.nc", 
#'  package = "tidync", mustWork = TRUE)
#' tidync(ufile)
#' hyper_tbl_cube(tidync(ufile))
#' @return `dplyr::tbl_cube`
#' @export
hyper_tbl_cube <- function(x, ...) {
  UseMethod("hyper_tbl_cube")
}
#' @name hyper_tbl_cube
#' @export
#' @importFrom stats setNames
hyper_tbl_cube.tidync <- function(x, ...) {
  active_names <- tibble::tibble(dim = as.integer(
    gsub("^D", "", unlist(strsplit(active(x), ",")))))
  dim_names <- active_names %>% 
    inner_join(x[["dimension"]] %>% 
                 dplyr::filter(active), c("dim" = "id")) %>% 
    dplyr::pull(.data$name)
  trans <- x[["transforms"]][dim_names]
  lfun <- function(inm) {
    trans[[inm]] %>% 
      dplyr::filter(.data$selected) %>% 
      dplyr::pull(inm)
  }
  ldims <- lapply(dim_names, lfun)
  structure(list(mets = hyper_array(x, ...), 
                 dims = setNames(ldims, dim_names)), 
            class = "tbl_cube")
}
#' @name hyper_tbl_cube
#' @export
hyper_tbl_cube.character <- function(x, ...) {
  tidync(x) %>% hyper_filter(...) %>% hyper_tbl_cube()
} 


