#' tidy netcdf
#' 
#' Function to extract all metadata from a NetCDF, for use in subsequent operations. By default
#' the first *shape* encountered is  `activate`d. 
#' 
#' Any NetCDF with variable arrays should work. Files with compound types are not yet supported. We
#' haven't explored HDF5 per se, so any feedback is appreciated. 
#' @param x path to a NetCDF file
#' @param what (optional) character or bare name of variable to `activate`
#' @export
tidync <- function(x, ...) {
  UseMethod("tidync")
}

#' @examples
#' cf <- raadtools::cpolarfiles()
#' nc <- tidync(cf$fullname[1])
#' print(nc)
#' @name tidync
#' @export
tidync.character <- function(x, what) {
    fexists <- file.exists(x)
   if (!fexists) warning(sprintf("cannot find file: \n%s", x))
       meta <- ncmeta::nc_meta(x)
       out <- structure(list(file = tibble::tibble(dsn = x), 
                             grid = shapes(meta) , 
                             dimension = dimensions(meta), 
                             variable = meta$variable),
                        
                        class = "tidync")
       ## we can't activate nothing
       if (nrow(out$grid) < 1) return(out)
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
#' @name tidync
#' @export
#' @importFrom dplyr %>% arrange distinct inner_join
print.tidync <- function(x, ...) {
  ushapes <- dplyr::distinct(x$grid, grid) %>% 
    dplyr::arrange(desc(nchar(grid)))
  nshapes <- nrow(ushapes)
  cat(sprintf("\nData Source (%i): %s ...\n", nrow(x$file), paste(head(basename(x$file$dsn), 2), collapse = ", ")))
  cat(sprintf("\nGrids (%i) <dimension family> : <associated variables> \n\n", nshapes))
  active_sh <- active(x)
  nms <- if(!is.null(ushapes$grid)) nchar(ushapes$grid) else 0
  longest <- sprintf("[%%i]   %%%is", -max(nms))
  estimatebigtime <- x$grid %>% 
    dplyr::filter(grid == active(x)) %>% 
    dplyr::inner_join(x$variable, c("variable" = "name")) %>% 
    dplyr::distinct(dimids) %>% 
    dplyr::inner_join(x$dimension, c("dimids" = "id"))

  ## hack to assume always double numeric 
  ## TODO because could be integer after load
  estimatebigtime <- format(prod(estimatebigtime$length))
  ##print_bytes(            * 8)
  for (ishape in seq_len(nshapes)) {
    #ii <- ord[ishape]
    cat(sprintf(longest, ishape, ushapes$grid[ishape]), ": ")

    cat(paste((x$grid %>% inner_join(ushapes[ishape, ], "grid"))$variable, collapse = ", "))
    if ( ushapes$grid[ishape] == active_sh) cat("    **ACTIVE GRID** (", estimatebigtime, 
                                                sprintf(" value%s per variable)", ifelse(estimatebigtime > 1, "s", "")))
    cat("\n")
  }
  dims <- x$dimension
  cat(sprintf("\nDimensions (%i): \n", nrow(dims)))
  dimension_print <- if (nrow(dims) > 0) {
    format(dims %>% dplyr::mutate(dimension = paste0("D", id)) %>% 
             dplyr::select(dimension, id, name, length, unlim), n = Inf)
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
