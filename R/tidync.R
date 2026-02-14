#' Tidy NetCDF
#'
#' Connect to a NetCDF source and allow use of `hyper_*` verbs for slicing with
#' [hyper_filter()], extracting data with [hyper_array()] and  [hyper_tibble()
#' from an activated grid. By default the largest *grid* encountered is
#' activated, see[activate()].
#'
#' The print method for tidync includes a lot of information about which
#' variables exist on which dimensions, and if any slicing ([hyper_filter()])
#' operations have occurred these are summarized as 'start' and 'count'
#' modifications relative to the dimension lengths. See [print][print.tidync()]
#' for these details, and [hyper_vars][hyper_vars()] for programmatic access to
#' this information
#'
#' Many NetCDF forms are supported and tidync tries to reduce the interpretation
#' applied to a given source. The NetCDF system defines a 'grid' for storing
#' array data, where 'grid' is the array 'shape', or 'set of dimensions'). There
#' may be several grids in a single source and so we introduce the concept of
#' grid 'activation'. Once activated, all downstream tasks apply to the set of
#' variables that exist on that grid.
#'
#' NetCDF sources with numeric types are chosen by default, even if existing
#' 'NC_CHAR' type variables are on the largest grid. When read any 'NC_CHAR'
#' type variables are exploded into single character elements so that dimensions
#' match the source.
#'
#' @section Multi-source: When `x` is a character vector of length > 1 and
#'   `concat_dim` is specified, tidync builds a consolidated view across all
#'   sources. The first source is used as a template for grid structure, and
#'   the `concat_dim` coordinate values are read from each source and
#'   concatenated. Downstream operations (`hyper_filter`, `hyper_array`, etc.)
#'   work transparently across the collection.
#'
#'   Use `fast = TRUE` for large collections to skip full metadata validation
#'   of sources 2..N (only the `concat_dim` coordinate is read). Mismatches
#'   are detected lazily at data-read time.
#'
#'   For maximum speed, supply coordinate values directly via the list form
#'   `concat_dim = list(name = "time", values = <vector>)`. This opens only
#'   the first source (for the template) and builds the concat transform from
#'   the supplied values with zero additional file I/O. This is ideal when
#'   coordinate values are already available from a file database.
#'
#' @section Grids: A grid is an instance of a particular set of dimensions,
#'   which can be shared by more than one variable. This is not the 'rank' of a
#'   variable (the number of dimensions) since a single data set may have many
#'   3D variables composed of different sets of axes/dimensions. There's no
#'   formality around the concept of 'shape', as far as we know.
#'
#'   A dimension may have length zero, but this is a special case for a
#'   "measure" dimension, we think. (It doesn't mean the product of the
#'   dimensions is zero, for example).
#'
#' @section Limitations: Files with compound types are not yet supported and
#'   should fail gracefully. Groups are not yet supported.
#'
#'   We haven't yet explored 'HDF5' in detail, so any feedback is appreciated.
#'   Major use of compound types is made by \url{https://github.com/sosoc/croc}.
#'
#' @param x path to a NetCDF file, or a character vector of paths for
#'   multi-source access (requires `concat_dim`)
#' @param ... reserved for arguments to methods, currently ignored
#' @param what (optional) character name of grid (see `ncmeta::nc_grids`) or
#'   (bare) name of variable (see `ncmeta::nc_vars`) or index of grid to
#'   `activate`
#' @param concat_dim (optional) name of the dimension to concatenate across
#'   sources, or a list with elements `name` (dimension name) and `values`
#'   (vector of coordinate values, one per source). The list form avoids
#'   opening files 2..N entirely — useful when values are already known
#'   from a file database. Values can be numeric, Date, or POSIXct.
#' @param fast logical, if `TRUE` skip full metadata validation for sources
#'   after the first (only reads the `concat_dim` coordinate). Mismatches
#'   are detected at data-read time. Ignored when `concat_dim$values` is
#'   supplied.
#' @export
tidync <- function(x, what, ...) {
  UseMethod("tidync")
}

#' @examples
#' ## a SeaWiFS (S) Level-3 Mapped (L3m) monthly (MO) chlorophyll-a (CHL)
#' ## remote sensing product at 9km resolution (at the equator)
#' ## from the NASA ocean colour group in NetCDF4 format (.nc)
#' ## for 31 day period January 2008 (S20080012008031) 
#' f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#' ## skip on Solaris
#' if (!tolower(Sys.info()[["sysname"]]) == "sunos") {
#' tnc <- tidync(l3file)
#' print(tnc)
#' }
#' 
#' ## very simple Unidata example file, with one dimension
#' \dontrun{
#' uf <- system.file("extdata/unidata", "test_hgroups.nc", package = "tidync")
#' recNum <- tidync(uf) |> hyper_tibble()
#' print(recNum)
#' }
#' ## a raw grid of Southern Ocean sea ice concentration from IFREMER
#' ## it is 12.5km resolution passive microwave concentration values
#' ## on a polar stereographic grid, on 2 October 2017, displaying the 
#' ## "hole in the ice" made famous here:
#' ## https://tinyurl.com/ycbchcgn
#' ifr <- system.file("extdata/ifremer", "20171002.nc", package = "tidync")
#' ifrnc <- tidync(ifr)
#' ifrnc |> hyper_tibble(select_var = "concentration")
#'
#' ## multi-source: concatenate files along a dimension
#' \dontrun{
#' files <- c("sst_2020_01.nc", "sst_2020_02.nc", "sst_2020_03.nc")
#' tnc <- tidync(files, concat_dim = "time")
#' tnc
#' ## filter and read across all sources transparently
#' tnc |> hyper_filter(time = time > 18300) |> hyper_tibble()
#'
#' ## fast mode for large collections (skips full metadata scan)
#' all_files <- list.files("daily/", pattern = "\\.nc$", full.names = TRUE)
#' tnc_fast <- tidync(all_files, concat_dim = "time", fast = TRUE)
#'
#' ## zero file I/O: supply values from a file database (e.g. raadfiles)
#' ## only the first file is opened (for the template)
#' dates <- as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
#' tnc_db <- tidync(files, concat_dim = list(name = "time", values = dates))
#' ## filter directly on the values you supplied
#' tnc_db |> hyper_filter(time = time > as.Date("2020-01-01")) |> hyper_tibble()
#' }
#' @name tidync
#' @export
#' @importFrom ncmeta nc_meta
tidync.character <- function(x, what, ..., concat_dim = NULL, fast = FALSE) {
  if (length(x) > 1) {
    if (!is.null(concat_dim)) {
      # Unpack list form: list(name = "time", values = <vector>)
      concat_values <- NULL
      if (is.list(concat_dim)) {
        if (is.null(concat_dim$name)) {
          stop("list-form concat_dim must have a 'name' element")
        }
        concat_values <- concat_dim$values  # may be NULL
        concat_dim <- concat_dim$name
      }
      if (missing(what)) {
        return(tidync_multi(x, concat_dim = concat_dim,
                            concat_values = concat_values,
                            fast = fast, ...))
      } else {
        return(tidync_multi(x, what = what, concat_dim = concat_dim,
                            concat_values = concat_values,
                            fast = fast, ...))
      }
    }
    if (!isTRUE(getOption("tidync.silent"))) {
      warning("only one source allowed, first supplied chosen")
    }
    x <- x[1L]
  }
  fexists <- file.exists(x)
  
  if (!fexists) {
     if (!isTRUE(getOption("tidync.silent"))) {
    message(sprintf("not a file: \n' %s '\n\n... attempting remote connection\n", 
                x))
     }
  }
  meta <- tryCatch(
    list(result = ncmeta::nc_meta(x), error = NULL),
    error = function(e) list(result = NULL, error = e)
  )

  if (is.null(meta$result)) {
    stop(meta$error)
  }

  bad_dim <- nrow(meta$result$dimension) < 1
  bad_var <- nrow(meta$result$variable) < 1
  if (is.null(bad_dim) || is.na(bad_dim) || length(bad_dim) < 1) bad_dim <- TRUE
  if (is.null(bad_var) || is.na(bad_var) || length(bad_var) < 1) bad_var <- TRUE

  
  if (bad_dim) {
      if (!isTRUE(getOption("tidync.silent"))) {

       warning("no dimensions found")
      }
  }
  if (bad_var) {
      if (!isTRUE(getOption("tidync.silent"))) {
        warning("no variables found")
      }
  }
  if (bad_dim && bad_var) {
    stop("no variables or dimensions \n  
         (is this a source with compound-types? Try h5, rhdf5, or hdf5r)")
  }
  if (!fexists) {
     if (!isTRUE(getOption("tidync.silent"))) {
    message("Connection succeeded.")    
     }
  }
  meta <- meta$result
  variable <- dplyr::mutate(meta[["variable"]], active = FALSE)

  out <- list(source = meta$source, 
              axis = meta$axis, 
              grid = meta$grid,
              dimension = meta$dimension, 
              variable = variable,
              extended = meta$extended,
              attribute = meta$attribute)
  out$transforms <- hyper_transforms(out, all = TRUE)

  out <- structure(out,           class = "tidync")
  ## we can't activate nothing
  if (nrow(out$axis) < 1) return(out)
  if (missing(what)) {
    varg  <- first_numeric_var(out)
    gg <- tidyr::unnest(out$grid, cols = "variables")
    
    what <- gg$grid[match(varg, gg$variable)]
  }
  out <- activate(out, what)

  out
}

first_numeric_var <- function(x) {
  priorityvar <-   x$axis |> 
    dplyr::inner_join(x$dimension, c("dimension" = "id")) |> 
    dplyr::inner_join(x$variable, c("variable" = "name")) |> 
      dplyr::arrange(.data$type == "NC_CHAR", -.data$ndims)
  if (nrow(priorityvar) < 1) {
     return(priorityvar$variable[1L])
  }
  priorityvar$variable[1L]
}

## ---- Multi-source constructor and helpers ----

#' Build a tidync object from multiple sources concatenated along one dimension.
#'
#' This is an internal constructor called by `tidync.character()` when `x` has
#' length > 1 and `concat_dim` is specified. It builds a template from the
#' first source, reads the `concat_dim` coordinate from each source, and
#' stitches them into a consolidated transforms table.
#'
#' @param sources character vector of file paths or URIs
#' @param what optional grid or variable name to activate
#' @param concat_dim name of the dimension to concatenate along
#' @param fast if TRUE, skip full metadata validation of sources 2..N
#' @param ... passed to `tidync()` for the template
#' @return tidync object with multi-source transforms
#' @noRd
tidync_multi <- function(sources, what, concat_dim, concat_values = NULL,
                         fast = FALSE, ...) {
  stopifnot(is.character(sources), length(sources) > 0)
  stopifnot(is.character(concat_dim), length(concat_dim) == 1L)

  # Build template from first source
  if (missing(what)) {
    template <- tidync(sources[1L], ...)
  } else {
    template <- tidync(sources[1L], what = what, ...)
  }

  # Validate concat_dim exists in the template
  if (!concat_dim %in% names(template$transforms)) {
    stop(sprintf("concat_dim '%s' not found in source dimensions: %s",
                 concat_dim,
                 paste(names(template$transforms), collapse = ", ")))
  }

  # Source table
  src_table <- tibble::tibble(
    source_id = seq_along(sources),
    source    = sources
  )

  # Template's concat transform (source 1)
  t1 <- template$transforms[[concat_dim]]

  if (!is.null(concat_values)) {
    # ---- Values-supplied path: zero file I/O for sources 2..N ----
    concat_all <- build_concat_from_values(
      concat_dim, concat_values, sources, t1
    )
  } else {
    # ---- File-reading path (original) ----
    concat_all <- build_concat_from_files(
      concat_dim, sources, t1, template, fast
    )
  }

  # Assemble the multi-source object
  out <- template
  out$source <- src_table
  out$transforms[[concat_dim]] <- concat_all
  out$concat_dim <- concat_dim
  out$fast_mode <- fast

  # Update the dimension table: concat dim length is now the total
  cdim_idx <- which(out$dimension$name == concat_dim)
  out$dimension$length[cdim_idx] <- nrow(concat_all)

  # Re-run update_slices to set start/count from the new transforms
  out <- update_slices(out)

  out
}

#' Build concat transform from user-supplied values (zero file I/O).
#'
#' Assumes one step per source (length(values) == length(sources)).
#' The supplied values become the coordinate column that hyper_filter
#' operates on.
#'
#' @param concat_dim dimension name (character)
#' @param concat_values vector of coordinate values (numeric, Date, POSIXct, ...)
#' @param sources character vector of file paths
#' @param t1 template concat transform (from first source)
#' @return tibble with the consolidated concat transform
#' @noRd
build_concat_from_values <- function(concat_dim, concat_values, sources, t1) {
  n_sources <- length(sources)
  n_values <- length(concat_values)

  if (n_values != n_sources) {
    stop(sprintf(
      paste("length of concat_dim$values (%d) must match number of",
            "sources (%d) (one value per file)"),
      n_values, n_sources))
  }

  # Put the user's values directly in the coordinate column.
  # This means hyper_filter(time = time > X) works on whatever type
  # the user supplied (numeric, Date, POSIXct).
  concat_all <- tibble::tibble(
    placeholder__ = concat_values,
    index = seq_len(n_values),
    local_index = rep(1L, n_values),
    source_id = seq_len(n_values),
    id = t1$id[1L],
    name = concat_dim,
    coord_dim = t1$coord_dim[1L],
    selected = TRUE
  )
  names(concat_all)[1L] <- concat_dim

  # Add timestamp column if the template has one, or if values are temporal
  is_temporal <- inherits(concat_values, "POSIXt") ||
                 inherits(concat_values, "Date")

  if (is_temporal) {
    concat_all$timestamp <- format(concat_values)
  } else if ("timestamp" %in% names(t1)) {
    concat_all$timestamp <- rep(NA_character_, n_values)
  }

  concat_all
}

#' Build concat transform by reading coordinate values from files.
#'
#' This is the original file-reading path. Opens each source to read
#' the concat_dim coordinate, optionally validates shared dimensions.
#'
#' @param concat_dim dimension name
#' @param sources file paths
#' @param t1 template concat transform
#' @param template tidync object from first source
#' @param fast skip validation if TRUE
#' @return tibble with the consolidated concat transform
#' @noRd
build_concat_from_files <- function(concat_dim, sources, t1, template, fast) {

  t1$local_index <- t1$index
  t1$source_id <- 1L

  concat_transforms <- vector("list", length(sources))
  concat_transforms[[1L]] <- t1

  has_timestamp <- "timestamp" %in% names(t1)
  concat_has_coords <- t1$coord_dim[1L]

  for (i in seq_along(sources)[-1L]) {
    meta_i <- NULL
    if (!fast) {
      meta_i <- tryCatch(
        ncmeta::nc_meta(sources[i]),
        error = function(e) stop(sprintf("failed to read metadata from '%s': %s",
                                         sources[i], conditionMessage(e)))
      )
      validate_against_template(meta_i, template, concat_dim, sources[i])
    }

    # Read the concat_dim coordinate values (or generate index sequence)
    if (concat_has_coords) {
      coord_vals <- nc_get(sources[i], concat_dim)
    } else {
      if (!is.null(meta_i)) {
        n_i <- meta_i$dimension$length[meta_i$dimension$name == concat_dim]
      } else {
        n_i <- nc_dim_len(sources[i], concat_dim)
      }
      coord_vals <- seq_len(n_i)
    }
    n_i <- length(coord_vals)

    ti <- tibble::tibble(
      placeholder__ = coord_vals,
      local_index = seq_len(n_i),
      source_id = i,
      index = NA_integer_,
      id = t1$id[1L],
      name = concat_dim,
      coord_dim = t1$coord_dim[1L],
      selected = TRUE
    )
    names(ti)[1L] <- concat_dim

    if (has_timestamp) {
      if (!fast) {
        ext_i <- meta_i$extended
        time_rows <- which(ext_i$name == concat_dim)
        cftime_obj <- NULL
        if (length(time_rows) > 0L) {
          candidate <- ext_i$time[[time_rows[1L]]]
          if (is.environment(candidate) || inherits(candidate, "CFtime")) {
            cftime_obj <- candidate
          }
        }
        if (!is.null(cftime_obj)) {
          ti$timestamp <- CFtime::as_timestamp(cftime_obj)
        } else {
          ti$timestamp <- rep(NA_character_, n_i)
        }
      } else {
        ti$timestamp <- rep(NA_character_, n_i)
      }
    }

    concat_transforms[[i]] <- ti
  }

  concat_all <- do.call(rbind, concat_transforms)
  concat_all$index <- seq_len(nrow(concat_all))
  concat_all
}

#' Validate a source's metadata against the template
#'
#' Checks that shared dimensions have matching lengths and that the active
#' variables are present.
#'
#' @param meta_i ncmeta::nc_meta result for the source being checked
#' @param template the tidync object built from the first source
#' @param concat_dim name of the concatenation dimension (excluded from checks)
#' @param source_label file path for error messages
#' @noRd
validate_against_template <- function(meta_i, template, concat_dim, source_label) {
  # Check shared dimensions have the same length
  shared_dims <- template$dimension[template$dimension$name != concat_dim, ]

  for (j in seq_len(nrow(shared_dims))) {
    dname <- shared_dims$name[j]
    len_template <- shared_dims$length[j]
    dim_i <- meta_i$dimension
    len_i <- dim_i$length[dim_i$name == dname]

    if (length(len_i) == 0L) {
      stop(sprintf("dimension '%s' not found in source '%s'",
                   dname, source_label))
    }
    if (len_i != len_template) {
      stop(sprintf(
        "dimension '%s' has length %d in '%s' but %d in template ('%s')",
        dname, len_i, source_label, len_template,
        template$source$source[1L]))
    }
  }

  # Check that active variables exist
  template_vars <- template$variable$name[template$variable$active]
  source_vars <- meta_i$variable$name
  missing_vars <- setdiff(template_vars, source_vars)
  if (length(missing_vars) > 0L) {
    stop(sprintf("variables %s missing from source '%s'",
                 paste(missing_vars, collapse = ", "), source_label))
  }
}

#' Print tidync object
#'
#' Provide a summary of variables and dimensions, organized by their 'grid' (or
#' 'shape') and with a summary of any slicing operations provided as 'start' and
#' 'count' summaries for each dimension in the active grid.
#'
#' See [tidync][tidync()] for detail about the object, and
#' [hyper_vars][hyper_vars()] for programmatic access to the active grid's
#' variable and dimension information.
#'
#' The print summary is organized in two sections, the first is available grids
#' (sets of dimensions) and their associated variables, the second is the
#' dimensions, separated into active and inactive. All dimensions may be active
#' in some NetCDF sources.
#'
#' Individual *active* dimensions include the following components: * 'dim'    -
#' dimension label, D0, D1, D2, ... * 'name'   - dimension name * 'length' -
#' size of the dimension * 'min'    - minimum value of the dimension * 'max' -
#' maximum value of the dimension * 'start'  - start index of subsetting *
#' 'count'  - length of subsetting index * 'dmin'   - minimum value of the
#' subset dimension * 'dmax'   - maximum value of the subset dimension * 'unlim'
#' - indicates whether dimension is unlimited (spread across other files,
#' usually the time-step) * 'coord_dim' - indicates whether dimension is a
#' coordinate-dimension (i.e. listed as a 1-D grid)
#'
#' The *inactive* dimension summary does not include 'start', 'count', 'dmin',
#' 'dmax' as these are identical to the values of 1, 'length', 'min', 'max' when
#' no array subsetting has been applied.
#' @param x NetCDF object
#'
#' @param ... reserved
#'
#' @name print.tidync
#' @export
#' @importFrom dplyr  arrange distinct inner_join desc
#' @importFrom utils head
#' @importFrom rlang .data
#' @examples
#' argofile <- system.file("extdata/argo/MD5903593_001.nc", package = "tidync")
#' argo <- tidync(argofile)
#' print(argo)
#' 
#' ## the print is modified by choosing a new grid or running filters
#' argo |> activate("D7,D9,D11,D8")
#' 
#' argo |> hyper_filter(N_LEVELS = index > 300)
print.tidync <- function(x, ...) {
  ushapes <- dplyr::distinct(x$grid, .data$grid) |> 
             dplyr::arrange(desc(nchar(.data$grid)))
  nshapes <- nrow(ushapes)
  cat(sprintf("\nData Source (%i): %s ...\n", nrow(x$source), 
          paste(utils::head(basename(x$source$source), 2), collapse = ", ")))
  if (!is.null(x$concat_dim)) {
    ct <- x$transforms[[x$concat_dim]]
    n_sel <- sum(ct$selected)
    n_tot <- nrow(ct)
    cat(sprintf("Concatenated along '%s' (%i/%i steps, %i source%s%s)\n",
                x$concat_dim, n_sel, n_tot, nrow(x$source),
                ifelse(nrow(x$source) > 1, "s", ""),
                if (isTRUE(x$fast_mode)) ", fast mode" else ""))
  }
  cat(sprintf("\nGrids (%i) <dimension family> : <associated variables> \n\n", 
              nshapes))
  if (nrow(ushapes) < 1L) {
    cat("No recognizable dimensions or variables \n 
        (... maybe HDF5? Consider 'rhdf5' package from Bioconductor.)\n")
    cat("\nStandard ncdump -h output follows for reference: \n\n")
    RNetCDF::print.nc(RNetCDF::open.nc(x$source$source))
    return(invisible(NULL))  
  }
  active_sh <- active(x)
  nms <- if(nrow(ushapes) > 0) nchar(ushapes$grid) else 0
  longest <- sprintf("[%%i]   %%%is", -max(nms))
  vargrids <- tidyr::unnest(x$grid, cols = "variables")
  
  estimatebigtime <- vargrids |> 
    dplyr::filter(.data$grid == active(x)) |> 
    dplyr::inner_join(x$axis, "variable", multiple = "all") |> 
    dplyr::inner_join(x$dimension, c("dimension" = "id"), multiple = "all") |> 
    dplyr::distinct(.data$dimension, .data$length)
  
  ## hack to assume always double numeric 
  ## TODO because could be integer after load
  estimatebigtime <- prod(estimatebigtime$length)

  for (ishape in seq_len(nshapes)) {
    #ii <- ord[ishape]
    cat(sprintf(longest, ishape, ushapes$grid[ishape]), ": ")
    
    cat(paste((vargrids |> 
                 dplyr::inner_join(ushapes[ishape, ], "grid"))$variable, 
              collapse = ", "))
    if ( ushapes$grid[ishape] == active_sh) cat("    **ACTIVE GRID** (", 
                                                format(estimatebigtime), 
                                            sprintf(" value%s per variable)", 
                                ifelse(estimatebigtime > 1, "s", "")))
    cat("\n")
  }
  dims <- x$dimension
  nms <- names(x$transforms)
  ## handle case where value is character
  for (i in seq_along(x$transforms)) {
    if (!is.numeric(x$transforms[[nms[i]]][[nms[i]]])) {
      x$transforms[[nms[i]]][[nms[i]]] <- NA_integer_
    }
  }
  ranges <- setNames(lapply(nms, function(a) {
    range(x$transforms[[a]][[a]])
  }), nms)
  filter_ranges <- setNames(lapply(nms, function(a) {
    tran <- dplyr::filter(x$transforms[[a]], .data$selected) 
    range(tran[[a]])
  }
  ), nms)

  filter_ranges <- do.call(rbind, filter_ranges)
  ranges <- do.call(rbind, ranges)

  idxnm <- match(names(x$transforms), dims$name)
  dims$dmin <- dims$dmax <- dims$min <- dims$max <- NA_real_
  ## fix tidync/issues/84
  ## idxnm was used on the LHS here as well, garbling the order
  
  dims[idxnm, c("dmin", "dmax")] <- as.data.frame(filter_ranges)
  dims[idxnm, c("min", "max")] <- as.data.frame(ranges)
  dimension_print <- ""
  dims_active <- dims$active
  if (nrow(dims) > 0) { 
    alldims <- dims |> dplyr::mutate(dim = paste0("D", .data$id)) |> 
      dplyr::select("dim", "id", "name", "length", 
                    "min", "max", "start", "count", 
                    "dmin", "dmax", "active", "unlim", 
                    "coord_dim") |> 
      dplyr::arrange(desc(.data$active), .data$id)
    
  dimension_active <-  format(alldims |> 
                              dplyr::filter(.data$active) |> 
                              dplyr::mutate(id = NULL, active = NULL), n = Inf)
  dimension_other <- format(alldims |> dplyr::filter(!.data$active) |> 
                            dplyr::select("dim", "name", "length", 
                                          "min", "max", "unlim",
                                          "coord_dim"), n = Inf)
    
  }

  if (any(!dims_active)) {
    cat(sprintf("\nDimensions %i (%i active): \n", nrow(dims), 
                sum(dims_active)))
  } else {
    cat(sprintf("\nDimensions %i (all active): \n", nrow(dims)))
  }
  
  dp <- dimension_active[-grep("# A tibble:", dimension_active)]
  cat(" ", "\n")
  for (i in seq_along(dp)) cat(dp[i], "\n")
  if (any(!dims_active)) {
  cat(" ", "\nInactive dimensions:\n")
  dp2 <- dimension_other[-grep("# A tibble:", dimension_other)]
  cat(" ", "\n")
  for (i in seq_along(dp2)) cat(dp2[i], "\n")
  }
  invisible(NULL)
}
