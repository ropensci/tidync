
#' tidy netcdf
#' 
#' Function to extract all metadata from a NetCDF, for use in subsequent operations. By default
#' the first variable encountered is  `activate`d. 
#' 
#' Any NetCDF with variable arrays should work. Files with compound types are not yet supported. We
#' haven't explored HDF5 per se, so any feedback is appreciated. 
#' @param x path to a NetCDF file
#' @param what (optional) character or bare name of variable to `activate`
#' @export
tidync <- function(x, ...) {
  UseMethod("tidync")
}
#' @name tidync
#' @export
tidync.tidyfile <- function(x, ...) {
  out <- structure(list(file = x, shape = shapes(x$meta[[1]])), class = "tidync")
  attr(out, "active_shape") <- out$shape$shape[1]
out
}
#' @name tidync
#' @export
tidync.data.frame <- function(x, ...) {
  tidync(tidyfile(x))
}

#' Print NetCDF object
#' 
#' print S3 method
#' 
#' Prints a summary of variables and dimensions, organized by their 'shape'. Shape is
#' an instance of a particular set of dimensions, which can be shared by more than one
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
  ushapes <- distinct(x$shape, shape) %>% arrange(desc(nchar(shape)))
  nshapes <- nrow(ushapes)
  cat(sprintf("Shapes (%i): \n", nshapes))
  active_sh <- attr(x, "active_shape")
  for (ishape in seq_len(nshapes)) {
    #ii <- ord[ishape]
    cat(sprintf("[%s]", ushapes$shape[ishape]), ": ")

    cat(paste((x$shape %>% inner_join(ushapes[ishape, ], "shape"))$variable, collapse = ", "))
    if ( ushapes$shape[ishape] == active_sh) cat("    **ACTIVE SHAPE**")
    cat("\n")
  }
  invisible(NULL)
}

# cf <- raadtools::cpolarfiles()
# nc <- tidync(cf)
# print_(nc)


# tidync <- function(x, what) {
#   ## TODO support NetCDF, ncdf4, RNetCDF, raster, anything with a file behind it
#   if (!is.character(x)) stop("'x' must be a file")
#   fexists <- file.exists(x)
#   if (!fexists) stop(sprintf("cannot find file: \n%s", x))
#   x <- structure(unclass(ncdump::NetCDF(x)), class = "tidync")
#   x$variable$shape <- shapes(x)
#   if (missing(what)) what <- x$variable$name[1L]
#   activate(x, what)
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
