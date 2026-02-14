#' Extract NetCDF data as an array
#'
#' Extract the raw array data as a list of  one or more arrays. This can be the
#' entire variable/s or after dimension-slicing using [hyper_filter()]
#' expressions. This is a delay-breaking function and causes data to be read 
#' from the source into R native arrays. This list of arrays is 
#' lightly classed as [tidync_data], with methods for [print()] and [tidync()]. 
#'
#' The function [hyper_array()] is used by [hyper_tibble()] and [hyper_tbl_cube()]
#' to actually extract data arrays from NetCDF, if a result would be particularly large
#' there is a check made and user-opportunity to cancel. This is controllable as an 
#' option `getOption('tidync.large.data.check')`, and can be set to never check with
#' `options(tidync.large.data.check = FALSE)`. 
#' 
#' The function [hyper_array()] will act on an existing tidync object or a source
#' string.
#'
#' By default all variables in the active grid are returned, use `select_var` to
#' specify one or more desired variables.
#'
#' The transforms are stored as a list of tables in an attribute `transforms`,
#' access these with [hyper_transforms()].
#' @param x NetCDF file, connection object, or [tidync] object
#' @param drop collapse degenerate dimensions, defaults to `TRUE`
#' @param ... passed to [hyper_filter()]
#' @param select_var optional vector of variable names to select
#' @param raw_datavals logical to control whether scaling in the NetCDF is
#'   applied or not
#' @param force ignore caveats about large extraction and just do it
#'
#' @export
#' @aliases tidync_data
#' @seealso [print.tidync_data] for a description of the print summary, 
#' [hyper_tbl_cube()] and [hyper_tibble()] which are also delay-breaking 
#' functions that cause data to be read 
#' @examples
#' f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#'
#' ## extract a raw list by filtered dimension
#' library(dplyr)
#' araw1 <- tidync(l3file) |>
#'  hyper_filter(lat = between(lat, -78, -75.8), 
#'               lon = between(lon, 165, 171)) |>
#'  hyper_array()
#'
#' araw <- tidync(l3file) |> 
#'          hyper_filter(lat = abs(lat) < 10, 
#'                      lon = index < 100) |>
#'   hyper_array()
#'
#' ## hyper_array will pass the expressions to hyper_filter
#' braw <- tidync(l3file) |> 
#'   hyper_array(lat = abs(lat) < 10, lon = index < 100)
#'
#' ## get the transforms tables (the axis coordinates)
#' lapply(attr(braw, "transforms"), 
#'    function(x) nrow(dplyr::filter(x, selected)))
#' ## the selected axis coordinates should match in order and in size
#' lapply(braw, dim)
hyper_array <- function(x, select_var = NULL, ..., 
                        raw_datavals = FALSE, force = FALSE, drop = TRUE) {
  UseMethod("hyper_array")
}

## alias
#' @name hyper_array
#' @export
hyper_slice <- function(x, select_var = NULL, ..., 
                        raw_datavals = FALSE, force = FALSE, drop = TRUE) {
  if (!isTRUE(getOption("tidync.silent"))) {
   warning("hyper_array should be used instead of hyper_slice")
  }
  hyper_array(x = x, select_var = select_var, ..., 
              raw_datavals = raw_datavals, force = force, drop = drop)
}

#' @name hyper_array
#' @export
hyper_array.tidync <- function(x, select_var = NULL, ..., 
                          raw_datavals = FALSE, force = FALSE, drop = TRUE) {
  x <- hyper_filter(x, ...) 
  variable <- x[["variable"]] |> dplyr::filter(active)
  varname <- unique(variable[["name"]])
  ## hack to get the order of the indices of the dimension
  ordhack <- 1 + as.integer(unlist(strsplit(gsub("D", "", 
                          dplyr::filter(x$grid, .data$grid == active(x)) |> 
                          # dplyr::slice(1L) |> THERE'S ONLY EVER ONE ACTIVE GRID
                          dplyr::pull(.data$grid)), ",")))
  dimension <- x[["dimension"]] |> dplyr::slice(ordhack)
  ## ensure dimension is in order of the dims in these vars
  axis <- x[["axis"]] |> dplyr::filter(variable %in% varname)
  ## dimension order must be same as axis
  START <- dimension$start
  COUNT <- dimension$count
 # browser()
  if (is.null(select_var))   {
    varnames <- varname
  } else {
    if (!all(select_var %in% variable[["name"]])) {
      bad <- base::setdiff(select_var, variable[["name"]])
      
      select_var <- base::intersect(select_var, variable[["name"]])
      if (length(select_var) < 1) stop("no select_var variables available")
      if (!isTRUE(getOption("tidync.silent"))) {
        warning(sprintf("some select_var variables not found, and ignored:\n %s",
                        paste(bad, collapse = ",")))
      }
    }
    ## todo, make this quosic?
    varnames <- select_var
  }

  #browser()
  if (interactive() && !force && prod(COUNT) * length(varnames) * 4 > 1e9) {
    opt <- getOption("tidync.large.data.check")
    if (!isTRUE(opt)) opt <- FALSE
    if (opt) {
      message("please confirm data extraction, Y(es) to proceed ... use 'force = TRUE' to avoid size check\n (see '?hyper_array')")
      mess <- sprintf("pretty big extraction, (%1$.0f*%2$i values [%3$s]*%2$i)", 
                      prod(COUNT), length(varnames), paste(COUNT, collapse = ", "))
      yes <- utils::askYesNo(mess)
      if (!yes) {
        stop("extraction cancelled by user", call. = FALSE)
      }
    }
  }
  
  ## Avoid opening file on disk multiple times for multiple variables
  ## If concat_dim is set but not part of the active grid, fall through
  ## to single-source (all sources are identical for shared dimensions)
  use_multi <- !is.null(x$concat_dim) && x$concat_dim %in% dimension$name
  if (!use_multi) {
    ## ---- Single source path (original) ----
    con <- suppressWarnings(ncdf4::nc_open(x$source$source[1]))
    on.exit(ncdf4::nc_close(con), add = TRUE)
    datalist <- lapply(varnames, function(vara) {
      ncdf4::ncvar_get(con, vara, start = START, count = COUNT, 
                       raw_datavals = raw_datavals, collapse_degen = FALSE)
    })
  } else {
    ## ---- Multi-source path ----
    datalist <- read_multi_source(x, varnames, dimension, START, COUNT,
                                  raw_datavals = raw_datavals)
  }

  ## Get dimension names from the transforms. Use "timestamp" instead of "time"
  transforms <- active_axis_transforms(x)
  dn <- lapply(transforms, function(trans) {
    ts <- suppressWarnings(trans[["timestamp"]])
    if (is.null(ts)) trans[[1]][trans$selected] else ts[trans$selected]
  })

  ## If some (but not all) of the variables defined on the grid are NC_CHAR then
  ## the NC_CHAR variables read here have to be split into characters to
  ## maintain consistent dimensionality with arrays of other data types
  ## (disregarding the esoteric possibility that a grid is used both for numeric
  ## data and for some text application).
  ## If all variables defined on the grid are NC_CHAR then don't split the read
  ## variables here but drop the first dimension from dn before applying
  ## dimnames. This is related to how NC_CHAR data is stored in NetCDF files. 
  ## The result is the string array as read directly from the file, with reduced
  ## array dimensions.
  grid_vars <- unlist(x$grid$variables[which(x$grid$grid == active(x))])
  var_dt <- x$variable$type[which(x$variable$name %in% grid_vars)]
  if (all(var_dt == "NC_CHAR")) dn <- dn[-1]
  else if (any(var_dt == "NC_CHAR")) {
    char_vars <- variable$type[match(varnames, variable$name)] == "NC_CHAR"
    if (any(char_vars)) {
      idx <- which(char_vars)
      for (i in seq_along(idx)) {
        ii <- idx[i]
        datalist[[ii]] <- array(unlist(strsplit(datalist[[ii]], "")),
                                dimension$count)
      }
    }
  }
  
  ## Apply dimnames
  datalist <- lapply(datalist, function(d) {dimnames(d) <- dn; d})
  
  ## Drop any degenerate dimensions, if requested and needed
  if (drop && any(lengths(dn) == 1)) datalist <- lapply(datalist, drop)
  
  structure(datalist, names = varnames, transforms = transforms, 
            source = x$source, concat_dim = x$concat_dim,
            class = "tidync_data")
}

#' @name hyper_array
#' @export
hyper_array.character <- function(x, select_var = NULL, ...,
                                  raw_datavals = FALSE, force = FALSE, drop = TRUE) {
  tidync(x) |> 
  hyper_filter(...) |>  
  hyper_array(select_var = select_var, raw_datavals = raw_datavals, drop = drop)
}

## ---- Multi-source reader ----

#' Read data from multiple NetCDF sources and concatenate along one dimension.
#'
#' Decomposes the global selection into per-source local slabs, reads each,
#' and concatenates along the concat dimension.
#'
#' @param x tidync object with concat_dim set
#' @param varnames character vector of variable names to read
#' @param dimension tibble of dimensions in axis order (from hyper_array)
#' @param START global start vector
#' @param COUNT global count vector
#' @param raw_datavals passed to ncvar_get
#' @return list of arrays (same structure as single-source path)
#' @noRd
read_multi_source <- function(x, varnames, dimension, START, COUNT,
                              raw_datavals = FALSE) {

  concat_dim <- x$concat_dim
  concat_trans <- x$transforms[[concat_dim]]
  selected_trans <- concat_trans[concat_trans$selected, ]

  # Which sources do we actually need?
  needed_sources <- sort(unique(selected_trans$source_id))

  if (length(needed_sources) == 0L) {
    stop("no sources selected after filtering")
  }

  # Which position is the concat dim in the dimension order?
  concat_pos <- which(dimension$name == concat_dim)

  # Per-source: compute local start/count for the concat dim
  source_slabs <- lapply(needed_sources, function(sid) {
    rows <- selected_trans[selected_trans$source_id == sid, ]
    local_start <- min(rows$local_index)
    local_end   <- max(rows$local_index)
    local_count <- local_end - local_start + 1L

    # Build the full start/count vectors for this source
    # Shared dims keep the global start/count, concat dim gets local
    s <- START
    c <- COUNT
    s[concat_pos] <- local_start
    c[concat_pos] <- local_count

    src_path <- x$source$source[x$source$source_id == sid]

    list(source_id = sid,
         source = src_path,
         start = s, count = c)
  })

  # Read from each source: open once, validate once, read all variables
  # Result: list of lists, outer = source, inner = variable
  per_source <- lapply(source_slabs, function(slab) {
    con <- suppressWarnings(ncdf4::nc_open(slab$source))
    on.exit(ncdf4::nc_close(con), add = TRUE)

    # Fast-mode lazy validation (once per source)
    if (isTRUE(x$fast_mode)) {
      validate_slab_compat(con, x, concat_dim)
    }

    # Read all variables from this source
    lapply(varnames, function(vara) {
      ncdf4::ncvar_get(con, vara, start = slab$start, count = slab$count,
                       raw_datavals = raw_datavals, collapse_degen = FALSE)
    })
  })

  # Transpose: list-of-sources-of-vars -> list-of-vars-of-sources, then abind
  datalist <- lapply(seq_along(varnames), function(vi) {
    arrays <- lapply(per_source, function(src) src[[vi]])
    abind_along(arrays, along = concat_pos)
  })

  datalist
}

#' Concatenate arrays along a specified dimension.
#'
#' Minimal implementation to avoid importing the abind package for one function.
#'
#' @param arrays list of arrays with identical dimensions except along `along`
#' @param along integer, the dimension to concatenate along
#' @return a single array
#' @noRd
abind_along <- function(arrays, along) {
  if (length(arrays) == 1L) return(arrays[[1L]])

  d1 <- dim(arrays[[1L]])
  ndim <- length(d1)

  total_along <- sum(vapply(arrays, function(a) dim(a)[along], integer(1)))
  out_dim <- d1
  out_dim[along] <- total_along

  out <- array(vector(typeof(arrays[[1L]]), 0L), dim = out_dim)

  pos <- 1L
  for (a in arrays) {
    n <- dim(a)[along]
    # Build index list: list(TRUE, TRUE, pos:(pos+n-1), TRUE, ...)
    idx <- rep(list(TRUE), ndim)
    idx[[along]] <- seq.int(pos, pos + n - 1L)
    out <- do.call(`[<-`, c(list(out), idx, list(a)))
    pos <- pos + n
  }
  out
}

#' Validate that a source's shared dimensions match the template.
#'
#' Called at read time when fast_mode = TRUE, since metadata validation
#' was skipped at construction time. Uses the ncdf4 connection which is
#' already open.
#'
#' @param con ncdf4 connection object
#' @param x tidync object
#' @param concat_dim name of the concat dimension (skip this one)
#' @noRd
validate_slab_compat <- function(con, x, concat_dim) {
  shared_dims <- x$dimension[x$dimension$name != concat_dim, ]
  for (i in seq_len(nrow(shared_dims))) {
    dname <- shared_dims$name[i]
    expected_len <- shared_dims$length[i]
    file_dim <- con$dim[[dname]]
    if (is.null(file_dim)) {
      stop(sprintf(
        paste("fast mode: dimension '%s' not found in '%s'.",
              "Re-run with fast = FALSE to validate all sources."),
        dname, con$filename))
    }
    if (file_dim$len != expected_len) {
      stop(sprintf(
        paste("fast mode: dimension '%s' has length %d in '%s'",
              "(expected %d from template).",
              "Re-run with fast = FALSE to validate all sources."),
        dname, file_dim$len, con$filename, expected_len))
    }
  }
}
