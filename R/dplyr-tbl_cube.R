#' dplyr tbl cube from NetCDF
#'
#' @param x filename
#' @param ... arguments for `hyper_filter`
#'
#' @return `dplyr::tbl_cube`
#' @export
#'
#' @examples
#' f <- "S20092742009304.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#' (cube <- hyper_tbl_cube(tidync(l3file) %>% 
#' activate(chlor_a), lon = lon > 107, lat = abs(lat) < 30))
#' library(ggplot2)
#' scl <- function(x) {rng <- range(x, na.rm = TRUE); (x - rng[1])/diff(rng)}
#' library(palr)
#' pal <- chlPal(palette = TRUE)
#' sfg <- scale_fill_gradientn(values = scl(head(pal$breaks, -1)), 
#'      colours = pal$cols)
#' ggplot(tibble::as_tibble(cube) %>% dplyr::filter(!is.na(chlor_a))) +  
#' geom_raster(aes(lon, lat, fill = chlor_a)) + sfg  + coord_equal() #+ 
#' #geom_polygon(data = ggplot2::map_data("world", 
#' #xlim = c(100, 180), 
#' #ylim = c(-30, 30)), aes(long, lat, group = group, fill = NULL))
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
