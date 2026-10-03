## ---- Lazy arrays (altarr) ----
##
## hyper_array(lazy = TRUE) returns, for each variable, an 'altarr' array:
## an ordinary double or integer vector with dim and dimnames, but backed by
## ALTREP so values are read from the NetCDF source(s) chunk by chunk, only
## when base R asks for them. The chunk reader below follows the altarr fetch
## contract: it receives a matrix of 0-based chunk coordinates (one row per
## chunk) and returns one column-major vector per row, clipped at the edge.

#' Build a list of lazy arrays, one per variable
#'
#' @param x tidync object (filtered)
#' @param varnames variables to read
#' @param dimension tibble of dimensions in axis order (from hyper_array)
#' @param START,COUNT global start and count vectors, in axis order
#' @param raw_datavals passed to ncdf4::ncvar_get
#' @param dn dimnames, one element per axis
#' @param drop drop degenerate dimensions?
#' @param use_multi read from multiple sources along concat_dim?
#' @return list of altarr arrays
#' @noRd
lazy_datalist <- function(x, varnames, dimension, START, COUNT, raw_datavals,
                          dn, drop, use_multi) {
  if (!requireNamespace("altarr", quietly = TRUE)) {
    stop("'lazy = TRUE' requires the 'altarr' package, ",
         "install with remotes::install_github(\"hypertidy/altarr\")",
         call. = FALSE)
  }
  types <- x$variable$type[match(varnames, x$variable$name)]
  if (any(types %in% c("NC_CHAR", "NC_STRING"))) {
    stop("'lazy = TRUE' supports numeric variables only, not: ",
         paste(varnames[types %in% c("NC_CHAR", "NC_STRING")], collapse = ", "),
         call. = FALSE)
  }
  START <- as.integer(START)
  COUNT <- as.integer(COUNT)

  ## which global index along the concat dim comes from which source and
  ## which local index (single source: one source, local = global)
  concat <- NULL
  if (use_multi) {
    concat_trans <- x$transforms[[x$concat_dim]]
    sel <- concat_trans[concat_trans$selected, ]
    concat <- list(pos = which(dimension$name == x$concat_dim),
                   source = x$source$source[match(sel$source_id,
                                                  x$source$source_id)],
                   local = as.integer(sel$local_index))
    ## selected steps may skip whole sources: the array spans the selected
    ## steps, not their bounding range
    COUNT[concat$pos] <- length(concat$local)
  }
  first_source <- if (use_multi) concat$source[1L] else x$source$source[1L]

  keep <- if (drop) COUNT > 1L else rep(TRUE, length(COUNT))
  ## a single value still needs one dimension
  if (!any(keep)) keep[1L] <- TRUE

  con <- suppressWarnings(ncdf4::nc_open(first_source))
  on.exit(ncdf4::nc_close(con), add = TRUE)
  lapply(varnames, function(vara) {
    var <- con$var[[vara]]
    chunk <- lazy_chunk_shape(var$chunksizes, COUNT)
    type <- lazy_value_type(con, var, START, COUNT, concat, raw_datavals)
    fetch <- nc_chunk_fetch(source = first_source, varname = vara,
                            start = START, count = COUNT, chunk = chunk,
                            keep = keep, raw_datavals = raw_datavals,
                            concat = concat)
    altarr::altarr(COUNT[keep], chunk[keep], fetch, dimnames = dn[keep],
                   type = type)
  })
}

#' Chunk shape for a lazy array
#'
#' The variable's own NetCDF chunking where it has one, otherwise
#' (contiguous storage, NetCDF classic) the selection split along its
#' slowest-varying dimensions so that each chunk is a contiguous block of
#' about `getOption("tidync.lazy.chunk_values", 2^20)` values.
#'
#' @param chunksizes from ncdf4 (R order), or NA
#' @param count the selection's count, in R order
#' @noRd
lazy_chunk_shape <- function(chunksizes, count) {
  count <- as.integer(count)
  if (length(chunksizes) == length(count) && !anyNA(chunksizes) &&
      all(chunksizes > 0)) {
    return(pmin(as.integer(chunksizes), count))
  }
  target <- getOption("tidync.lazy.chunk_values", 2^20)
  chunk <- count
  for (k in rev(seq_along(chunk))) {
    if (prod(as.numeric(chunk)) <= target) break
    inner <- prod(as.numeric(chunk[-k]))
    chunk[k] <- as.integer(max(1, min(chunk[k], floor(target / inner))))
  }
  chunk
}

#' The R type ncdf4 gives this variable: "double" or "integer"
#'
#' Floating point variables are always double. For integer types ncdf4
#' returns integer unless it applies scale_factor or add_offset, so rather
#' than mirror its rules one value is read and its type taken.
#' @noRd
lazy_value_type <- function(con, var, START, COUNT, concat, raw_datavals) {
  if (var$prec %in% c("float", "double")) return("double")
  start <- START
  if (!is.null(concat)) start[concat$pos] <- concat$local[1L]
  v <- ncdf4::ncvar_get(con, var$name, start = start,
                        count = rep(1L, length(COUNT)),
                        raw_datavals = raw_datavals, collapse_degen = FALSE)
  if (is.integer(v)) "integer" else "double"
}

#' A fetch function (altarr contract) for one NetCDF variable
#'
#' A factory: the returned function's environment holds only a path (or the
#' per-index source paths of a multi-source view) and a few integer vectors,
#' so a lazy array can be saved with saveRDS() and read in a new session.
#'
#' @param source path or URL of the (single) source
#' @param varname variable name
#' @param start,count the selection, 1-based, in R (ncdf4) order
#' @param chunk chunk shape over the full selection
#' @param keep which dimensions the array has (FALSE for dropped ones, whose
#'   count is 1)
#' @param raw_datavals passed to ncdf4::ncvar_get
#' @param concat NULL, or list(pos, source, local) for a multi-source view
#' @noRd
nc_chunk_fetch <- function(source, varname, start, count, chunk, keep,
                           raw_datavals, concat = NULL) {
  force(source); force(varname); force(start); force(count); force(chunk)
  force(keep); force(raw_datavals); force(concat)
  function(chunks) {
    chunks <- matrix(as.integer(chunks), ncol = sum(keep))
    pieces <- vector("list", nrow(chunks))
    slabs <- list()
    for (r in seq_len(nrow(chunks))) {
      cc <- integer(length(count))
      cc[keep] <- chunks[r, ]
      off <- cc * chunk
      n <- pmin(chunk, count - off)
      runs <- if (is.null(concat)) {
        list(list(source = source, start = start + off, count = n))
      } else {
        concat_runs(concat, start, off, n)
      }
      pieces[[r]] <- length(slabs) + seq_along(runs)
      slabs <- c(slabs, runs)
    }
    vals <- read_lazy_slabs(slabs, varname, raw_datavals)
    lapply(pieces, function(i) {
      if (length(i) == 1L) return(as.vector(vals[[i]]))
      as.vector(abind_along(vals[i], along = concat$pos))
    })
  }
}

#' Split one chunk of a multi-source view into per-source slabs
#'
#' Along the concat dimension the chunk covers global positions
#' off + 1 .. off + n of the selection; consecutive positions from the same
#' source with consecutive local indices are read as one slab.
#' @noRd
concat_runs <- function(concat, start, off, n) {
  p <- concat$pos
  g <- off[p] + seq_len(n[p])
  src <- concat$source[g]
  loc <- concat$local[g]
  brk <- c(TRUE, src[-1L] != src[-length(src)] | diff(loc) != 1L)
  run <- cumsum(brk)
  lapply(split(seq_along(g), run), function(i) {
    s <- start + off
    cn <- n
    s[p] <- loc[i[1L]]
    cn[p] <- length(i)
    list(source = src[i[1L]], start = s, count = cn)
  })
}

#' Read slabs of one variable, opening each source once
#'
#' Slabs are grouped by source; groups are read in parallel when mirai
#' daemons are active (one fetch call is one round trip), sequentially
#' otherwise.
#' @return list of arrays, in slab order
#' @noRd
read_lazy_slabs <- function(slabs, varname, raw_datavals) {
  srcs <- vapply(slabs, function(s) s$source, "")
  groups <- split(seq_along(slabs), factor(srcs, unique(srcs)))
  jobs <- lapply(groups, function(i) slabs[i])
  res <- if (length(jobs) > 1L && has_mirai_daemons()) {
    mirai::collect_mirai(
      mirai::mirai_map(unname(jobs), read_slab_group,
                       .args = list(varname = varname,
                                    raw_datavals = raw_datavals)),
      options = ".stop")
  } else {
    lapply(jobs, read_slab_group, varname = varname, raw_datavals = raw_datavals)
  }
  out <- vector("list", length(slabs))
  out[unlist(groups, use.names = FALSE)] <- unlist(res, recursive = FALSE,
                                                   use.names = FALSE)
  out
}

#' Read slabs of one variable from one source (self-contained, for mirai)
#' @noRd
read_slab_group <- function(group, varname, raw_datavals) {
  con <- suppressWarnings(ncdf4::nc_open(group[[1L]]$source))
  on.exit(ncdf4::nc_close(con), add = TRUE)
  lapply(group, function(s) {
    ncdf4::ncvar_get(con, varname, start = s$start, count = s$count,
                     raw_datavals = raw_datavals, collapse_degen = FALSE)
  })
}
