#' A dplyr cube tbl 
#' 
#' Produce a [tbl_cube][dplyr::tbl_cube()] from NetCDF. This is a 
#' delay-breaking function and causes data to be read from the source
#' into the tbl cube format defined in the [dplyr][dplyr::tbl_cube] 
#' package. 
#' 
#' The tbl cube is a very general and arbitrarily-sized array that 
#' can be used with tidyverse functionality. Dimension coordinates are
#' stored with the tbl cube, derived from the grid 
#' [transforms][hyper_transforms()]. 
#'
#' @param x tidync object
#' @param ... arguments for [hyper_filter()]
#' @seealso [hyper_array()] and [hyper_tibble()] which are also delay-breaking 
#' functions that cause data to be read 
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
#'  
#' ## some versions of NetCDF don't support this file
#' ## (4.1.3 https://github.com/hypertidy/tidync/issues/82)
#' group_nc <- try(tidync(ufile), silent = TRUE)
#' if (!inherits(group_nc, "try-error")) {
#'  res <-  hyper_tbl_cube(tidync(ufile))
#'  print(res)
#' } else {
#'  ## the error was
#'  writeLines(c(group_nc))
#' }
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


