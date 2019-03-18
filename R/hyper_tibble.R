#' Extract NetCDF data as an expanded table.
#'
#' Extract the raw array data as an expanded data frame. This can be the entire
#' variable/s or after dimension-slicing using `hyper_filter` expressions with
#' dimension values expanded appropriately for each element in the arrays (one
#' row per element).
#'
#' The function `hyper_array` will act on an existing tidync object or a source
#' string.
#'
#' By default all variables in the active grid are returned, use `select_var` to
#' limit.
#'
#' @param x NetCDF file, connection object, or `tidync` object
#' @param ... arguments to `hyper_filter``
#' @param na.rm if `TRUE` these rows are not included in the output when all
#'   variables are `NA`
#' @return a `tbl_df`
#' @export
#' @importFrom dplyr %>%
#' @export %>%
#' @seealso [hyper_array()] and [hyper_tbl_cube()]  which are also delay-breaking 
#' functions that cause data to be read 
#' @examples
#' l3file <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' f <- system.file("extdata", "oceandata", l3file, package= "tidync")
#' rnc <- tidync(f)
#' hyper_filter(rnc)
#' library(dplyr)
#' lapply(hyper_array(f, lat = lat > 0, lon = index > 3000), dim)
#'
#'  ht <- hyper_tibble(rnc) %>%
#'  filter(!is.na(chlor_a))
#' ht
#' library(ggplot2)
#' ggplot(ht %>% filter(!is.na(chlor_a)),
#' aes(x = lon, y = lat, fill = chlor_a)) + geom_tile()
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
  
  slabs <- hyper_array(x, ...)
  if (na.rm) all_na <- Reduce(`&`, lapply(slabs, 
                                          function(a) is.na(as.vector(a))))
  total_prod <- prod(dim(slabs[[1]]))
out <- tibble::as_tibble(lapply(slabs, as.vector))
  
  prod_dims <- 1
  trans <- attr(slabs, "transforms")

  for (i in seq_along(trans)) {
    nm <- names(trans)[i]
    nr <- sum(trans[[i]]$selected)
    out[[nm]] <- rep(dplyr::filter(trans[[nm]], .data$selected)[[nm]], 
                     each = prod_dims, length.out = total_prod)
    prod_dims <- prod_dims * nr
  }
  if (na.rm) out <- dplyr::filter(out, !all_na)
 out
}
