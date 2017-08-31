#' hyper tibble
#'
#' @param x object to tibbulate
#' @param ... arguments to `hyper_filter``
#'
#' @return a `tbl_df`
#' @export
#' @importFrom dplyr %>% 
#' @export %>% 
#' @examples
#' l3file <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump")
#' rnc <- tidync(l3file)
#' hyper_filter(rnc)
#' library(dplyr)
#' lapply(hyper_slice(l3file, lat = lat > 0, lon = index > 3000), dim)
#' 
#'  ht <- hyper_tibble(rnc) %>% 
#'  filter(!is.na(chlor_a)) 
#' ht   
#' library(ggplot2)
#' ggplot(ht %>% filter(!is.na(chlor_a)), 
#' aes(x = lon, y = lat, fill = chlor_a)) + geom_point()
#' 
hyper_tibble <- function(x, ...) {
  UseMethod("hyper_tibble")
}
#' @name hyper_tibble
#' @importFrom ncdump NetCDF
#' @export
hyper_tibble.character <- function(x, ...) {
  tidync(x) %>% hyper_filter(...) %>% hyper_tibble()
}
#' @name hyper_tibble
#' @export
hyper_tibble.tidync <- function(x, ...) {
  x %>% hyper_filter(...) %>% hyper_tibble()
}
#' @name hyper_tibble
#' @export
hyper_tibble.hyperfilter <- function(x, ...) {
  
  slab <- hyper_slice(x, ...)
  total_prod <- prod(dim(slab[[1]]))
  
  tib <- list()
  #okfilter <- rep(nomin_space$ok, length = total_prod)
  #okfilter <- rep(TRUE, total_prod)  ## hack for now
  okfilter <- TRUE
  ns <- attr(x, "nominal_space")
  apply_okfilter <- !is.null(ns)
  if (apply_okfilter) okfilter <- ns$ok
  #tib[[active(x)]] <- as.vector(slab)[okfilter]
  tib <- tibble::as_tibble(lapply(slab, as.vector))
  #tib <- tibble::as_tibble(tib)
  prod_dims <- 1
  
  for (i in seq_along(x)) {
    #    if (names(x)[i] == "nominal_space") next;
    nm <- names(x)[i]
    nr <- sum(x[[i]]$selected)
    if (apply_okfilter) {
      ## this is a slow down so don't do it if it's not needed
    tib[[nm]] <- rep(x[[nm]][[nm]], each = prod_dims, length.out = total_prod)[okfilter]
    } else {
      tib[[nm]] <- rep(dplyr::filter(x[[nm]], .data$selected)[[nm]], each = prod_dims, length.out = total_prod)
      
    }
    prod_dims <- prod_dims * nr
  }
  structure(tib, class = c("hyper_tbl", "tbl_df", "tbl", "data.frame"), 
            dims = names(x))
  
}
