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
#' @param x path to a NetCDF file
#' @param ... reserved for arguments to methods, currently ignored
#' @param what (optional) character name of grid (see `ncmeta::nc_grids`) or
#'   (bare) name of variable (see `ncmeta::nc_vars`) or index of grid to
#'   `activate`
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
#' recNum <- tidync(uf) %>% hyper_tibble()
#' print(recNum)
#' }
#' ## a raw grid of Southern Ocean sea ice concentration from IFREMER
#' ## it is 12.5km resolution passive microwave concentration values
#' ## on a polar stereographic grid, on 2 October 2017, displaying the 
#' ## "hole in the ice" made famous here:
#' ## https://tinyurl.com/ycbchcgn
#' ifr <- system.file("extdata/ifremer", "20171002.nc", package = "tidync")
#' ifrnc <- tidync(ifr)
#' ifrnc %>% hyper_tibble(select_var = "concentration")
#' @name tidync
#' @export
#' @importFrom ncmeta nc_meta
tidync.character <- function(x, what, ...) {
  if (length(x) > 1) {
    warning("only one source allowed, first supplied chosen")
    x <- x[1L]
  }
  fexists <- file.exists(x)
  
  if (!fexists) {
    message(sprintf("not a file: \n' %s '\n\n... attempting remote connection\n", 
                x))
  }
  safemeta <- purrr::safely(ncmeta::nc_meta)
  meta <- safemeta(x)

  if (is.null(meta$result)) {
    stop(meta$error)
  }

  bad_dim <- nrow(meta$result$dimension) < 1
  bad_var <- nrow(meta$result$variable) < 1
  if (is.null(bad_dim) || is.na(bad_dim) || length(bad_dim) < 1) bad_dim <- TRUE
  if (is.null(bad_var) || is.na(bad_var) || length(bad_var) < 1) bad_var <- TRUE

  if (bad_dim) {
    warning("no dimensions found")
  }
  if (bad_dim) {
    warning("no variables found")
  }
  if (bad_dim && bad_var) {
    stop("no variables or dimension recognizable \n  
         (is this a source with compound-types? Try h5, rhdf5, or hdf5r)")
  }
  if (!fexists) message("Connection succeeded.")      
  meta <- meta$result
  variable <- dplyr::mutate(meta[["variable"]], active = FALSE)

  out <- list(source = meta$source, 
              axis = meta$axis, 
              grid = meta$grid,
              dimension = meta$dimension, 
              variable = variable)
  out$transforms <- hyper_transforms(out, all = TRUE)

  out <- structure(out,           class = "tidync")
  ## we can't activate nothing
  if (nrow(out$axis) < 1) return(out)
  if (missing(what)) {
    varg  <- first_numeric_var(out)
    if (utils::packageVersion("tidyr") > "0.8.3" ) {
      gg <- tidyr::unnest(out$grid, cols = c(.data$variables))
      
    } else {
      gg <- tidyr::unnest(out$grid)
    }
    what <- gg$grid[match(varg, gg$variable)]
   
  }
  out <- activate(out, what)

  out
}

first_numeric_var <- function(x) {
  priorityvar <-   x$axis %>% 
    dplyr::inner_join(x$dimension, c("dimension" = "id")) %>% 
    dplyr::inner_join(x$variable, c("variable" = "name")) %>% 
      dplyr::arrange(.data$type == "NC_CHAR", -.data$ndims)
  if (nrow(priorityvar) < 1) {
     return(priorityvar$variable[1L])
  }
  priorityvar$variable[1L]
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
#' @importFrom dplyr %>% arrange distinct inner_join desc
#' @importFrom utils head
#' @importFrom rlang .data
#' @examples
#' argofile <- system.file("extdata/argo/MD5903593_001.nc", package = "tidync")
#' argo <- tidync(argofile)
#' print(argo)
#' 
#' ## the print is modified by choosing a new grid or running filters
#' argo %>% activate("D7,D9,D11,D8")
#' 
#' argo %>% hyper_filter(N_LEVELS = index > 300)
print.tidync <- function(x, ...) {
  ushapes <- dplyr::distinct(x$grid, .data$grid) %>% 
    dplyr::arrange(desc(nchar(.data$grid)))
  nshapes <- nrow(ushapes)
  cat(sprintf("\nData Source (%i): %s ...\n", 
              nrow(x$source), 
          paste(utils::head(basename(x$source$source), 2), collapse = ", ")))
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
  nms <- if(nrow(ushapes) > 0)  nchar(ushapes$grid) else 0
  longest <- sprintf("[%%i]   %%%is", -max(nms))
  if (utils::packageVersion("tidyr") > "0.8.3" ) {
    vargrids <- tidyr::unnest(x$grid, cols = c(.data$variables))
  } else {
    vargrids <- tidyr::unnest(x$grid)
  }
  estimatebigtime <- vargrids %>% 
    dplyr::filter(.data$grid == active(x)) %>% 
    dplyr::inner_join(x$axis, "variable") %>% 
    dplyr::inner_join(x$dimension, c("dimension" = "id")) %>% 
    distinct(.data$dimension, .data$length)
  ## hack to assume always double numeric 
  ## TODO because could be integer after load
  estimatebigtime <- prod(estimatebigtime$length)

  for (ishape in seq_len(nshapes)) {
    #ii <- ord[ishape]
    cat(sprintf(longest, ishape, ushapes$grid[ishape]), ": ")
    
    cat(paste((vargrids %>% 
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
    alldims <- dims %>% dplyr::mutate(dim = paste0("D", .data$id)) %>% 
      dplyr::select(.data$dim, .data$id, .data$name, .data$length, 
                    .data$min, .data$max, .data$start, .data$count, 
                    .data$dmin, .data$dmax, .data$active, .data$unlim, 
                    .data$coord_dim) %>% 
      dplyr::arrange(desc(.data$active), .data$id)
    
  dimension_active <-  format(alldims %>% 
                              dplyr::filter(.data$active) %>% 
                              dplyr::mutate(id = NULL, active = NULL), n = Inf)
  dimension_other <- format(alldims %>% dplyr::filter(!.data$active) %>% 
                              dplyr::select(.data$dim, .data$name,   
                                            .data$length, .data$min, .data$max,
                                            
                                        .data$unlim, .data$coord_dim), n = Inf)
    
  }

  if (any(!dims_active)) {
    cat(sprintf("\nDimensions %i (%i active): \n", nrow(dims), 
                sum(dims_active)))
  } else {
    cat(sprintf("\nDimensions %i (all active): \n", nrow(dims)))
  }
  
  #browser()
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


