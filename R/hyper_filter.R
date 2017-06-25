
hyper_filter_Spatial <- function(x, y, ...) {
  #stop("nothing to see here")
  #warning("assuming first two dimensions are longitude-latitude ...")
  #dim_tabs <- hyper_filter(x)
  #xy_names <- names(dim_tabs)[1:2]
  #xy_grid <- as.matrix(expand.grid(x = dim_tabs[[1]][[xy_names[1]]], 
  #                        y = dim_tabs[[1]][[xy_names[1]]]))
  #over(y, SpatialPoints(xy_grid, proj4string = crs(y)))
}


#' Array subset by nse
#'
#' NSE arguments must be named as per the dimensions in the variable. This is a restrictive variant of `dplyr::filter`, 
#' with a syntax more like `dplyr::mutate`. This ensures that each element is named, so we know which dimension to 
#' apply this to, but also that the expression evaluated against can do some extra work for a nuanced test. 
#' @param x NetCDF object
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
hyper_filter <- function(x, ...) {
  UseMethod("hyper_filter")
}
#' @name hyper_filter
#' @export
#' @importFrom dplyr %>% mutate 
#' @importFrom forcats as_factor
hyper_filter.tidync <- function(x, ...) {
  
  dims <- x$grid %>% 
    dplyr::filter(grid == active(x)) %>% 
    dplyr::inner_join(x$axis, "variable") %>% 
    dplyr::inner_join(x$dimension, c("dimension" = "id")) %>% 
    dplyr::distinct(name, dimension) 
  ## potentially flaky here because the dimension-order is not
  ## necessarily the way the variable uses them, i.e. l3file
  ## is lon, lat but lat is 0 and lon is 1
  ## it appears that native order is correct, but this needs to come
  ## from ncmeta
  ##%>%  dplyr::arrange(dimension)
#  print("doom")
  ## dimensions don't necessarily have variables
  ## FIXME
if (!all(dims$name %in% x$variable$name)) warning("dims don't have values...we are going to error...")
# trans <- lapply(setNames(purrr::map(dims$name, ~nc_get(x$source$source, .)), dims$name), tibble::as_tibble)
 
  ## FIXME: this is slow, and possibly redundant if we use the ncdf4 con anyway, ncmeta should provide
  ## this as a an upfront resource
  trans0 <- setNames(lapply(dims$name, function(vname) tibble::as_tibble(list(value = nc_get(x$source$source, vname)) )), 
                     dims$name)
  
  trans <- trans0
#  still debugging here, so live with a copy
  for (i in seq_along(dims$name)) {
    names(trans[[i]]) <- dims$name[i]
    trans[[i]]$index <- seq_len(nrow(trans[[i]])) 
    trans[[i]]$id <- dims$dimension[i]
    trans[[i]]$name <- dims$name[i]
    trans[[i]]$filename <- x$file$dsn
  }
  
  quo_named <- rlang::quos(...)
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100'")
  quo_noname <- unname(quo_named)
  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    trans[[iname]] <- dplyr::filter(trans[[iname]], !!!quo_noname[i])
    if (nrow(trans[[iname]]) < 1L) stop(sprintf("subexpression for [%s] results in empty slice, no intersection specified", 
                                                iname))
  }
  #trans <- lapply(trans, function(ax) {ax$filename <- x$file$filename; ax})
  out <- hyper_filter(trans) %>% activate(active(x))
  ## FIXME: using attributes is a hack  https://github.com/hypertidy/tidync/issues/33
  attr(out, "source") <- x$source
  attr(out, "grid") <- x$grid
  out
}

#' @name hyper_filter
#' @export
hyper_filter.default <- function(x, ...) {
  structure(x, class = c("hyperfilter", class(x)))
}
#' @name hyper_filter
#' @export
hyper_filter.character <- function(x, ...) {
  tidync(x) %>% hyper_filter(...)
}
#' @name hyper_filter
#' @export
hyper_filter.hyperfilter <- function(x, ...) {
  stop("too many filters in the chain, you can't (yet) 'hyper_filter' a hyperfilter")
}
#' @name hyper_filter
#' @importFrom dplyr bind_rows funs group_by select summarize_all
#' @importFrom rlang .data
#' @export
print.hyperfilter <- function(x, ...) {
  source <- attr(x, "source")
  ## FIXME: hack on the first source available
  sourcename <- source$source[1L]
  summ <- dplyr::bind_rows(
    lapply(x,  function(a) dplyr::summarize_all(a %>% 
            dplyr::select(-.data$index, -.data$id) %>% group_by(.data$name)
            , dplyr::funs(min, max, length)))
            )
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
  on.exit(RNetCDF::close.nc(con), add = TRUE)
  con <- RNetCDF::open.nc(x)
  safe_get <- purrr::safely(nc_get.NetCDF)
  val <- safe_get(con, v)
  if (!is.null(val$result)) return(val$result)
  on.exit(ncdf4::nc_close(con4), add = TRUE)
  con4 <- ncdf4::nc_open(x, readunlim = FALSE, verbose = FALSE, auto_GMT = FALSE, suppress_dimvals = TRUE)
  safe_get4 <- purrr::safely(nc_get.ncdf4)
    val <- safe_get4(con4, v)
  if (!is.null(val$result)) {
    return(val$result)
    } else {
      stop("error")
    }
}
nc_get.NetCDF <- function(x, v) {
  RNetCDF::var.get.nc(x, v)
}


nc_get.ncdf4 <- function(x, v) {
  ncdf4::ncvar_get(con4, v)
}
