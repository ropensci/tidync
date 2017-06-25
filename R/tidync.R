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
#' #cf <- raadtools::cpolarfiles()
#' #nc <- tidync(cf$fullname[1])
#' #print(nc)
#' @name tidync
#' @export
#' @importFrom ncmeta nc_meta
tidync.character <- function(x, what, ...) {
    fexists <- file.exists(x)

   if (!fexists) cat(sprintf("not a file: \n' %s '\n\n... attempting remote connection", x))
      safemeta <- purrr::safely(ncmeta::nc_meta)
      meta <- safemeta(x)
       if (is.null(meta$result)) stop(meta$error)
       meta <- meta$result
       out <- structure(list(source = meta$source, 
                             axis = meta$axis, 
                             grid = meta$grid,
                             dimension = meta$dimension, 
                             variable = meta$variable),
                        
                        class = "tidync")
       ## we can't activate nothing
       if (nrow(out$axis) < 1) return(out)
       if (missing(what)) what <- 1
       out <- activate(out, what)
  out
}

#   x <- structure(unclass(ncdump::NetCDF(x)), class = "tidync")
#   x$variable$shape <- shapes(x)
#   if (missing(what)) what <- x$variable$name[1L]
#   activate(x, what)
# }


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
    dplyr::inner_join(x$dimension, c("dimension" = "id"))

  ## hack to assume always double numeric 
  ## TODO because could be integer after load
  estimatebigtime <- format(prod(estimatebigtime$length))
  ##print_bytes(            * 8)
  for (ishape in seq_len(nshapes)) {
    #ii <- ord[ishape]
    cat(sprintf(longest, ishape, ushapes$grid[ishape]), ": ")

    cat(paste((x$grid %>% dplyr::inner_join(ushapes[ishape, ], "grid"))$variable, collapse = ", "))
    if ( ushapes$grid[ishape] == active_sh) cat("    **ACTIVE GRID** (", estimatebigtime, 
                                                sprintf(" value%s per variable)", ifelse(estimatebigtime > 1, "s", "")))
    cat("\n")
  }
  dims <- x$dimension
  cat(sprintf("\nDimensions (%i): \n", nrow(dims)))
  dimension_print <- if (nrow(dims) > 0) {
    format(dims %>% dplyr::mutate(dimension = paste0("D", .data$id)) %>% 
             dplyr::select(.data$dimension, .data$id, .data$name, .data$length, .data$unlim), n = Inf)
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
