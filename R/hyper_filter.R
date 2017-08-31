

#' Array subset by nse
#'
#' NSE arguments must be named as per the dimensions in the variable. This is a restrictive variant of `dplyr::filter`, 
#' with a syntax more like `dplyr::mutate`. This ensures that each element is named, so we know which dimension to 
#' apply this to, but also that the expression evaluated against can do some extra work for a nuanced test. 
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
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100'")
  quo_noname <- unname(quo_named)
  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    SELECTION <- dplyr::filter(trans0[[iname]], !!!quo_noname[i])
    if (nrow(SELECTION) < 1L) stop(sprintf("subexpression for [%s] results in empty slice, no intersection specified", 
                                                 iname))
    
    trans0[[iname]]$selected <- trans0[[iname]]$index %in% SELECTION$index
  }
  out <- setNames(hyper_filter(trans0) %>% activate(active(.x)), names(trans0))
  ## FIXME: using attributes is a hack  https://github.com/hypertidy/tidync/issues/33
  attr(out, "source") <- .x$source
  attr(out, "grid") <- .x$grid
  attr(out, "nominal_space") <- NULL
  out
}

#' @name hyper_filter
#' @export
hyper_filter.default <- function(.x, ...) {
  structure(.x, class = c("hyperfilter", class(.x)))
}
#' @name hyper_filter
#' @export
hyper_filter.character <- function(.x, ...) {
  tidync(.x) %>% hyper_filter(...)
}
#' @name hyper_filter
#' @export
hyper_filter.hyperfilter <- function(.x, ...) {
  stop("too many filters in the chain, you can't (yet) 'hyper_filter' a hyperfilter")
}
#' @name hyper_filter
#' @param x tidync object to print
#' @importFrom dplyr bind_rows funs group_by select summarize_all
#' @importFrom rlang .data
#' @export
print.hyperfilter <- function(x, ...) {
  source <- attr(x, "source")
  ## FIXME: hack on the first source available
  sourcename <- source$source[1L]
  # summ <- dplyr::bind_rows(
  #    lapply(x,  function(a) dplyr::summarize_all(a %>% 
  #            dplyr::select(-.data$index, -.data$id) %>% group_by(.data$name, .data$coord_dim)
  #            , dplyr::funs(min, max, length)))
  #            )
  summ <- vector("list", length(x))
  for (i in seq_along(x)) {
    x[[i]] <- x[[i]] %>% dplyr::filter(.data$selected)
    if (is.numeric(x[[i]][[1]])) {
      summ[[i]] <- tibble::tibble(name = x[[i]]$name[1], coord_dim = x[[i]]$coord_dim[1], min = min(x[[i]][[1]]), max = max(x[[i]][[1]]), length = nrow(x[[i]]))
    } else {
      summ[[i]] <- tibble::tibble(name = x[[i]]$name[1], coord_dim = x[[i]]$coord_dim[1], min = NA_real_, max = NA_real_, length = nrow(x[[i]]))
    }
  }
  summ <- dplyr::bind_rows(summ)
  print(source)
  cat("filtered dimension summary: \n")
  print(summ)
  invisible(summ)
}

#' @importFrom ncdf4 nc_open nc_close ncvar_get
#' @importFrom RNetCDF open.nc close.nc var.get.nc
#' @importFrom purrr safely
nc_get <- function(x, v) {
  UseMethod("nc_get")
}
nc_get.character <- function(x, v) {
  con <- RNetCDF::open.nc(x)
  on.exit(RNetCDF::close.nc(con), add = TRUE)
  
  safe_get <- purrr::safely(nc_get.NetCDF)
  val <- safe_get(con, v)
  if (!is.null(val$result)) return(val$result)
  con4 <- NULL
  con4 <- ncdf4::nc_open(x, readunlim = FALSE, verbose = FALSE, auto_GMT = FALSE, suppress_dimvals = TRUE)
  on.exit(ncdf4::nc_close(con4), add = TRUE)
  safe_get4 <- purrr::safely(ncdf4::ncvar_get)
  val <- safe_get4(con4, v)
  if (!is.null(val$result)) {
    return(val$result)
  } else {
    #stop("error")
    warning("dimension has no coordinates, returning index")
    return()
  }
}
nc_get.NetCDF <- function(x, v) {
  RNetCDF::var.get.nc(x, v)
}


nc_get.ncdf4 <- function(x, v) {
  ncdf4::ncvar_get(x, v)
}
