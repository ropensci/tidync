#' hyper tibble
#'
#' @param x object to tibbulate
#' @param ... arguments to `hyper_filter``
#' @param na.rm if `TRUE` these rows are not included in the output when all variables are `NA`
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
hyper_tibble <- function(x, ..., na.rm = TRUE) {
  UseMethod("hyper_tibble")
}
#' @name hyper_tibble
#' @export
hyper_tibble.character <- function(x, ..., na.rm = TRUE) {
  tidync(x) %>% hyper_filter(...) %>% hyper_tibble()
}
#' @name hyper_tibble
#' @export
hyper_tibble.tidync<- function(x, ..., na.rm = TRUE) {
  
  slabs <- hyper_slice(x, ...)
  if (na.rm) all_na <- Reduce(`&`, lapply(slabs, function(a) is.na(as.vector(a))))
  total_prod <- prod(dim(slabs[[1]]))
  #okfilter <- TRUE
  #ns <- attr(x, "nominal_space")
  #apply_okfilter <- !is.null(ns)
  #if (apply_okfilter) okfilter <- ns$ok
  #tib[[active(x)]] <- as.vector(slab)[okfilter]
  out <- tibble::as_tibble(lapply(slabs, as.vector))
  
  prod_dims <- 1
  trans <- x[["transforms"]][x[["dimension"]][["active"]]]
  for (i in seq_along(trans)) {
    nm <- names(trans)[i]
    nr <- sum(trans[[i]]$selected)
    out[[nm]] <- rep(dplyr::filter(trans[[nm]], .data$selected)[[nm]], each = prod_dims, length.out = total_prod)
    prod_dims <- prod_dims * nr
  }
  if (na.rm) out <- dplyr::filter(out, !all_na)
 out
}
