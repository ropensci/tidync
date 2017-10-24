

#' Array subset by expression
#'
#' Filter arguments must be named as per the dimensions in the variable. This is a restrictive variant of `dplyr::filter`, 
#' with a syntax more like `dplyr::mutate`. This ensures that each element is named, so we know which dimension to 
#' apply this to, but also that the expression evaluated against can do some extra work for a nuanced test. 
#' 
#' There are special columns provided with each axis, one is 'index' so that exact matching can be
#' done by position, or to ignore the actual value of the coordinate. 
#' @param .x NetCDF file, connection object, or `tidync` object
#' @param ... currently ignored
#'
#' @return data frame
#' @export
#' @importFrom purrr map
#' @importFrom dplyr group_by mutate summarize
#' @examples
#' ## inst/dev/filtrate_early.R
#' ##http://rpubs.com/cyclemumner/tidyslab
#' # 
hyper_filter <- function(.x, ...) {
  UseMethod("hyper_filter")
}
#' @name hyper_filter
#' @export
#' @importFrom dplyr %>% mutate 
#' @importFrom forcats as_factor
#' @importFrom tibble as_tibble
hyper_filter.tidync <- function(.x, ...) {
  quo_named <- rlang::quos(...)
  trans0 <- active_axis_transforms(.x)
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100' or 'lat = index > 10'")
  quo_noname <- unname(quo_named)

  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    if (!iname %in% names(trans0)) {
      warning(sprintf("'%s' not found in active grid, ignoring", iname))
      next;
    }
    SELECTION <- dplyr::filter(trans0[[iname]], !!!quo_noname[i])
    if (nrow(SELECTION) < 1L) stop(sprintf("subexpression for [%s] results in empty slice, no intersection specified", 
                                                 iname))
    
    trans0[[iname]]$selected <- trans0[[iname]]$index %in% SELECTION$index
  }

  .x[["transforms"]][names(trans0)] <- trans0

  out <- update_slices(.x)
  
out
}

update_slices <- function(x) {
  
  transforms <- x[["transforms"]]
  starts <- unlist(lapply(transforms, function(axis) head(which(axis$selected), 1L)))
  ends <- unlist(lapply(transforms, function(axis) tail(which(axis$selected), 1L)))
  actual_counts <- unlist(lapply(transforms, function(axis) length(which(axis$selected))))
  counts <- ends - starts + 1L
  ## todo make this more informative
  if (!all(counts == actual_counts)) warning("arbitrary indexing within dimension is not yet supported")
                        
 idx <- match(names(starts), x[["dimension"]][["name"]])
 x[["dimension"]][["start"]][idx] <- starts
 x[["dimension"]][["count"]][idx] <- counts
 
#  x[["dimension"]] <- dplyr::inner_join(x[["dimension"]], tibble(name = names(starts), start = starts, count = counts), "name")
x
}
# ## @name hyper_filter
## ## @export
## hyper_filter.default <- function(.x, ...) {
##   structure(.x, class = c("hyperfilter", class(.x)))
## }
## ## @name hyper_filter
## ## @export
## hyper_filter.character <- function(.x, ...) {
##   tidync(.x) %>% hyper_filter(...)
## }
## ## @name hyper_filter
## ## @export
## hyper_filter.hyperfilter <- function(.x, ...) {
##   stop("too many filters in the chain, you can't (yet) 'hyper_filter' a hyperfilter")
## }
## ## @name hyper_filter
## ## @param x tidync object to print
## ## @importFrom dplyr bind_rows funs group_by select summarize_all
## ## @importFrom rlang .data
## ## @export
#' print.hyperfilter <- function(x, ...) {
#'   source <- attr(x, "source")
#'   ## FIXME: hack on the first source available
#'   sourcename <- source$source[1L]
#'   # summ <- dplyr::bind_rows(
#'   #    lapply(x,  function(a) dplyr::summarize_all(a %>%
#'   #            dplyr::select(-.data$index, -.data$id) %>% group_by(.data$name, .data$coord_dim)
#'   #            , dplyr::funs(min, max, length)))
#'   #            )
#'   summ <- vector("list", length(x))
#'   for (i in seq_along(x)) {
#'     x[[i]] <- x[[i]] %>% dplyr::filter(.data$selected)
#'     if (is.numeric(x[[i]][[1]])) {
#'       summ[[i]] <- tibble::tibble(name = x[[i]]$name[1], coord_dim = x[[i]]$coord_dim[1], min = min(x[[i]][[1]]), max = max(x[[i]][[1]]), length = nrow(x[[i]]))
#'     } else {
#'       summ[[i]] <- tibble::tibble(name = x[[i]]$name[1], coord_dim = x[[i]]$coord_dim[1], min = NA_real_, max = NA_real_, length = nrow(x[[i]]))
#'     }
#'   }
#'   summ <- dplyr::bind_rows(summ)
#'   print(source)
#'   cat("filtered dimension summary: \n")
#'   print(summ)
#'   invisible(summ)
#' }
