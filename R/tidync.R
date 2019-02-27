#' tidy netcdf
#' 
#' Function to extract all metadata from a NetCDF, for use in subsequent operations. By default
#' the first *shape* encountered is  `activate`d. 
#' 
#' Any NetCDF with variable arrays should work. Files with compound types are not yet supported and
#' should fail gracefully. 
#' 
#' We haven't yet explored HDF5 per se, so any feedback is appreciated. Major 
#' use of compound types is made by \url{https://github.com/sosoc/croc}
#' @param x path to a NetCDF file
#' @param ... reserved for arguments to methods, currently ignored
#' @param what (optional) character name of grid (see `ncmeta::nc_grids`) or (bare) name of variable (see `ncmeta::nc_vars`) or index of grid to `activate`
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
#' tnc <- tidync(l3file)
#' print(tnc)
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
  if (length(x) > 1) warning("multiple sources: only one source name allowed, ignoring all but the first")
  fexists <- file.exists(x)
  
  if (!fexists) cat(sprintf("not a file: \n' %s '\n\n... attempting remote connection\n", x))
  safemeta <- purrr::safely(ncmeta::nc_meta)
  meta <- safemeta(x)

  if (is.null(meta$result)) {
    cat("Oops, connection to source failed. \n")
    print(x)
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
    stop("no variables or dimension recognizable \n  (is this a source with compound-types? Try h5, rhdf5, or hdf5r)")
  }
  if (!fexists) cat("Connection succeeded. \n")      
  meta <- meta$result
  variable <- dplyr::mutate(meta[["variable"]], active = FALSE)

  out <- list(source = meta$source, 
              axis = meta$axis, 
              grid = meta$grid,
              dimension = meta$dimension, 
              variable = variable)
  out$transforms <- axis_transforms(out)

  out <- structure(out,           class = "tidync")
  ## we can't activate nothing
  if (nrow(out$axis) < 1) return(out)
  if (missing(what)) {
    varg  <- first_numeric_var(out)
    gg <- tidyr::unnest(out$grid)
    what <- gg$grid[match(varg, gg$variable)]
   
  }
  out <- activate(out, what)

  out
}

first_numeric_var <- function(x) {
  # grid <- x$grid
  # grid$type <- x$variable$type[match(grid$variable, x$variable$name)] 
  # grid <- dplyr::filter(grid, !type == "NC_CHAR")
  # if (nrow(grid) < 1) {
  #   warning("no non-NC_CHAR variables found (dimensionality does not make sense with CHAR, so beware)")
  #   return(1L)
  # }
  # 
  # match(grid$variable, x$grid$variable)[1L]

  priorityvar <-   x$axis %>% 
    dplyr::inner_join(x$dimension, c("dimension" = "id")) %>% 
    dplyr::inner_join(x$variable, c("variable" = "name")) %>% 
      dplyr::arrange(.data$type == "NC_CHAR", -.data$ndims)
  if (nrow(priorityvar) < 1) {
     return(priorityvar$variable[1L])
  }
  if (priorityvar$type[1] == "NC_CHAR") {
   warning("no non-NC_CHAR variables found (dimensionality does not make sense with CHAR, so beware)")
  }
  priorityvar$variable[1L]
}
read_groups <- function(src) {
  ncdf4::nc_open(src, readunlim = FALSE, verbose = FALSE, auto_GMT = FALSE, suppress_dimvals = TRUE)
}
## TBD we need
## support NetCDF, ncdf4, RNetCDF, raster, anything with a file behind it
#tidync.NetCDF
#tidync.ncdf4
# xtractomatic, rerddap?

#' Print NetCDF object
#' 
#' print S3 method
#' 
#' Prints a summary of variables and dimensions, organized by their 'shape' - "same shape" means "same grid". 
#' Shape is an instance of a particular set of dimensions, which can be shared by more than one
#' variable. This is not the 'rank' of a variable (the number of dimensions) since a single
#' data set may have many 3D variables composed of different sets of axes/dimensions. There's no
#' formality around the concept of 'shape', as far as we know. 
#' 
#' A dimension may have length zero, but this is a special case for a "measure" dimension, we think. (It 
#' doesn't mean the product of the dimensions is zero, for example).   
#' @param x NetCDF object
#'
#' @param ... reserved
#'
#' @name print.tidync
#' @export
#' @importFrom dplyr %>% arrange distinct inner_join desc
#' @importFrom utils head
#' @importFrom rlang .data
print.tidync <- function(x, ...) {
  ushapes <- dplyr::distinct(x$grid, .data$grid) %>% 
    dplyr::arrange(desc(nchar(.data$grid)))
  nshapes <- nrow(ushapes)
  cat(sprintf("\nData Source (%i): %s ...\n", 
              nrow(x$source), 
              paste(utils::head(basename(x$source$source), 2), collapse = ", ")))
  cat(sprintf("\nGrids (%i) <dimension family> : <associated variables> \n\n", nshapes))
  if (nrow(ushapes) < 1L) {
    cat("No recognizable dimensions or variables \n(... maybe HDF5? Consider 'rhdf5' package from Bioconductor.)\n")
    cat("\nStandard ncdump -h output follows for reference: \n\n")
    RNetCDF::print.nc(RNetCDF::open.nc(x$source$source))
    return(invisible(NULL))  
  }
  active_sh <- active(x)
  nms <- if(nrow(ushapes) > 0)  nchar(ushapes$grid) else 0
  longest <- sprintf("[%%i]   %%%is", -max(nms))
  vargrids <- tidyr::unnest(x$grid)
  estimatebigtime <- vargrids %>% 
    dplyr::filter(.data$grid == active(x)) %>% 
    dplyr::inner_join(x$axis, "variable") %>% 
    #dplyr::distinct(dimids) %>% 
    dplyr::inner_join(x$dimension, c("dimension" = "id")) %>% 
    distinct(.data$dimension, .data$length)
  ## hack to assume always double numeric 
  ## TODO because could be integer after load
  estimatebigtime <- prod(estimatebigtime$length)
  ##print_bytes(            * 8)
  for (ishape in seq_len(nshapes)) {
    #ii <- ord[ishape]
    cat(sprintf(longest, ishape, ushapes$grid[ishape]), ": ")
    
    cat(paste((vargrids %>% dplyr::inner_join(ushapes[ishape, ], "grid"))$variable, collapse = ", "))
    if ( ushapes$grid[ishape] == active_sh) cat("    **ACTIVE GRID** (", format(estimatebigtime), 
                                                sprintf(" value%s per variable)", ifelse(estimatebigtime > 1, "s", "")))
    cat("\n")
  }
  dims <- x$dimension
  cat(sprintf("\nDimensions (%i): \n", nrow(dims)))
  nms <- names(x$transforms)
  ## handle case where value is character
  for (i in seq_along(x$transforms)) {
    
    if (!is.numeric(x$transforms[[nms[i]]][[nms[i]]])) x$transforms[[nms[i]]][[nms[i]]] <- NA_integer_
  }
  ranges <- setNames(lapply(nms, function(a) {
    range(x$transforms[[a]][[a]])
  }), nms)
  filter_ranges <- setNames(lapply(nms, function(a) {
    tran <- dplyr::filter(x$transforms[[a]], .data$selected) 
    range(tran[[a]])
  }
  ), nms)
  # browser()
  filter_ranges <- do.call(rbind, filter_ranges)
  ranges <- do.call(rbind, ranges)
  
  # browser()
  idxnm <- match(names(x$transforms), dims$name)
  dims$dmin <- dims$dmax <- dims$min <- dims$max <- NA_real_
  dims[idxnm, c("dmin", "dmax")] <- as.data.frame(filter_ranges)[idxnm, ]
  dims[idxnm, c("min", "max")] <- as.data.frame(ranges)[idxnm, ]
  dimension_print <- ""
  if (nrow(dims) > 0) { 
  dimension_print <-  
    format(dims %>% dplyr::mutate(dim = paste0("D", .data$id)) %>% 
             dplyr::select(.data$dim, .data$id, .data$name, .data$length, .data$min, .data$max, .data$active, .data$start, .data$count, .data$dmin, .data$dmax, .data$unlim, .data$coord_dim,) %>% 
             dplyr::arrange(desc(.data$active), .data$id), n = Inf)
    
  }

  dp <- dimension_print[-grep("# A tibble:", dimension_print)]
  cat(" ", "\n")
  for (i in seq_along(dp)) cat(dp[i], "\n")
  invisible(NULL)
}


