

#' Subset NetCDF variable by expression
#'
#' The [hyper_filter()] acts on a [tidync] object by matching one or more
#' filtering expressions like with `dplyr::filter`. This allows us to lazily
#' specify a subset from a NetCDF array without pulling  any data. The modified
#' object may be printed to see the effects of subsetting, or saved for further
#' use.
#'
#' The function [hyper_filter()] will act on an existing tidync object or a 
#' source string.
#'
#' Filter arguments must be named as per the dimensions in the variable in form
#' `dimname = dimname < 10`. This is a restrictive variant of [dplyr::filter()],
#' with a syntax more like [dplyr::mutate()]. This ensures that each element is
#' named, so we know which dimension to apply this to, but also that the
#' expression evaluated against can do some extra work for a nuanced test.
#'
#' There are special columns provided with each axis, one is 'index' so that
#' exact matching can be done by position, or to ignore the actual value of the
#' coordinate. That means we can use a form like `dimname = index < 10` to
#' subset by position in the array index, without necessarily knowing the 
#' values along that dimension.
#' @param .x NetCDF file, connection object, or `tidync` object
#' @param ... currently ignored
#'
#' @return data frame
#' @export
#' @importFrom dplyr group_by mutate summarize
#' @examples
#' f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#' ## filter by value
#' tidync(l3file) %>% hyper_filter(lon = lon < 100)
#' ## filter by index
#' tidync(l3file) %>% hyper_filter(lon = index < 100)
#'
#' ## be careful that multiple comparisons must occur in one expression
#'  tidync(l3file) %>% hyper_filter(lon = lon < 100 & lon > 50)
#' 
#' ## filter in combination/s
#' tidync(l3file) %>% hyper_filter(lat = abs(lat) < 10, lon = index < 100)
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
  trans0 <- hyper_transforms(.x)
  if (any(nchar(names(quo_named)) < 1)) {
    stop("subexpressions must be in 'mutate' form, i.e.
           'lon = lon > 100' or 'lat = index > 10'")
  }
  quo_noname <- unname(quo_named)
  ## check that named subexpressions are unique
  names1 <- names(quo_named)
  if (length(unique(names1)) < length(names1)) warning("named expressions must be unique, found repeated names and only the later of a duplicate will be applied")
  for (i in seq_along(quo_named)) {
    iname <- names1[i]
    if (!iname %in% names(trans0)) {
      warning(sprintf("'%s' not found in active grid, ignoring", iname))
      next
    }
    SELECTION <- dplyr::filter(trans0[[iname]], !!!quo_noname[i])
    if (nrow(SELECTION) < 1L) {
      stop(sprintf("subexpression for [%s] results in empty slice", 
                                                 iname))
    }
    
    trans0[[iname]]$selected <- trans0[[iname]]$index %in% SELECTION$index
  }

  .x[["transforms"]][names(trans0)] <- trans0

  out <- update_slices(.x)
  
out
}


#' @export
hyper_filter.character <- function(.x, ...) {
  tidync(.x) %>% hyper_filter(...) 
}
update_slices <- function(x) {
  transforms <- x[["transforms"]]
 starts <- unlist(lapply(transforms, 
                         function(axis) head(which(axis$selected), 1L)))
  ends <- unlist(lapply(transforms, 
                        function(axis) utils::tail(which(axis$selected), 1L)))
  actual_counts <- unlist(lapply(transforms, 
                                 function(axis) length(which(axis$selected))))
  counts <- ends - starts + 1L
  ## todo make this more informative
  if (!all(counts == actual_counts)) {
    warning("arbitrary indexing within dimension is not yet supported")
  }                        
 idx <- match(names(starts), x[["dimension"]][["name"]])
 x[["dimension"]][["start"]] <- NA
 x[["dimension"]][["count"]] <- NA
 x[["dimension"]][["start"]][idx] <- starts
 x[["dimension"]][["count"]][idx] <- counts
 x
}
