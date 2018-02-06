#' tidy netcdf
#' 
#' Function to extract all metadata from a NetCDF, for use in subsequent operations. By default
#' the first *shape* encountered is  `activate`d. 
#' 
#' Any NetCDF with variable arrays should work. Files with compound types are not yet supported and
#' should fail gracefully. 
#' 
#' We haven't yet explored HDF5 per se, so any feedback is appreciated. Major 
#' use of compound types is made by https://github.com/mdsumner/roc
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
#' from the NASA ocean colour group in NetCDF4 format (.nc)
#' for 30 day period October 2009 (20092742009304) 
#' f <- "S20092742009304.L3m_MO_CHL_chlor_a_9km.nc"
#' l3file <- system.file("extdata/oceandata", f, package= "tidync")
#' tnc <- tidync(l3file)
#' print(tnc)
#' 
#' ## very simple Unidata example file, with one dimension
#' uf <- system.file("extdata/unidata", "test_hgroups.nc", package = "tidync")
#' tidync(uf)
#' 
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
  if (bad_dim) {
    warning("no dimensions found")
  }
  if (bad_dim) {
    warning("no variables found")
  }
  if (bad_dim & bad_var) stop("no variables or dimension recognizable (is this a source with compound-types?)")
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
  if (missing(what)) what <- 1
  out <- activate(out, what)
  
  out
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
  estimatebigtime <- x$grid %>% 
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
    
    cat(paste((x$grid %>% dplyr::inner_join(ushapes[ishape, ], "grid"))$variable, collapse = ", "))
    if ( ushapes$grid[ishape] == active_sh) cat("    **ACTIVE GRID** (", format(estimatebigtime), 
                                                sprintf(" value%s per variable)", ifelse(estimatebigtime > 1, "s", "")))
    cat("\n")
  }
  dims <- x$dimension
  cat(sprintf("\nDimensions (%i): \n", nrow(dims)))
  nms <- names(x$transforms)
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
  
  dims[c("sl_min", "sl_max")] <- as.data.frame(filter_ranges)[match(names(x$transforms), dims$name), ]
  dims[c("min", "max")] <- as.data.frame(ranges)[match(names(x$transforms), dims$name), ]
  
  dimension_print <- if (nrow(dims) > 0) {
    format(dims %>% dplyr::mutate(dim = paste0("D", .data$id)) %>% 
             dplyr::select(.data$dim, .data$id, .data$name, .data$length, .data$min, .data$max, .data$active, .data$start, .data$count, .data$sl_min, .data$sl_max, .data$unlim, .data$coord_dim,) %>% 
             dplyr::arrange(desc(.data$active), .data$id), n = Inf)
    
  } else {
    ""
  }
  #dp <- gsub("^ ?[0-9]?", "", dimension_print)  
  #dp <- gsub("^  ", "", dp)
  
  dp <- dimension_print[-grep("# A tibble:", dimension_print)]
  cat(" ", "\n")
  for (i in seq_along(dp)) cat(dp[i], "\n")
  #rownames(dims) <- paste0("D", dims$id)
  #print(dims)
  invisible(NULL)
}


# 
# # @name tidync
# # @export
# tidync.tidyfile <- function(x, ...) {
#   meta <- ncmeta::nc_meta(x$fullname[1L])
#   out <- structure(list(file = x, 
#                         shape = shapes(meta), 
#                         dimension = dimensions(meta)),
#                    
#                    class = "tidync")
#   activate(out, shapes(meta)$shape[1])
# }
# # @name tidync
# # @export
# tidync.data.frame <- function(x, ...) {
#   tidync(tidyfile(x))
# }


# print.tidync <- function(x, ...) {
#   form <- active(x)
#   vn <- c(form, setdiff(var_names(x), form))
#   if (length(vn)> 1) {
#     form <- sprintf("%s, (%s)", vn[1L],  paste(vn[-1L], collapse = ", "))
#   }
#   cat(sprintf("Variables: %s", form), "\n")
#   
#   cat(sprintf("Dimensions: \n", ""))
#   print(variable_dimensions(x) %>% 
#           inner_join(x$dimension %>% dplyr::transmute(.data$.dimension_, dimension_length = .data$len), ".dimension_"))
#   # print(x$dimension %>% 
#   #         dplyr::arrange(.data$id)  %>% 
#   #         transmute(.data$name, length = .data$len, 
#   #                   unlimited= .data$unlim ) %>% as.data.frame())
#   invisible(NULL)
# }
