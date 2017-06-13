
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
tidync <- function(x, what) {
  ## TODO support NetCDF, ncdf4, RNetCDF, raster, anything with a file behind it
  if (!is.character(x)) stop("'x' must be a file")
  fexists <- file.exists(x)
  if (!fexists) stop(sprintf("cannot find file: \n%s", x))
  x <- structure(unclass(ncdump::NetCDF(x)), class = "tidync")
  x$variable$shape <- shapes(x)
  if (missing(what)) what <- x$variable$name[1L]
  activate(x, what)
}
















#' Print NetCDF object
#' 
#' print S3 method
#' 
#' Prints a summary of variables and dimensions. 
#' @param x NetCDF object
#'
#' @param ... reserved
#'
#' @rdname print-NetCDF
#' @export
#' @importFrom dplyr %>% arrange transmute
print.tidync <- function(x, ...) {
  form <- active(x)
  vn <- c(form, setdiff(var_names(x), form))
  if (length(vn)> 1) {
    form <- sprintf("%s, (%s)", vn[1L],  paste(vn[-1L], collapse = ", "))
  }
  cat(sprintf("Variables: %s", form), "\n")
  
  cat(sprintf("Dimensions: \n", ""))
  print(variable_dimensions(x) %>% 
          inner_join(x$dimension %>% dplyr::transmute(.data$.dimension_, dimension_length = .data$len), ".dimension_"))
  # print(x$dimension %>% 
  #         dplyr::arrange(.data$id)  %>% 
  #         transmute(.data$name, length = .data$len, 
  #                   unlimited= .data$unlim ) %>% as.data.frame())
  invisible(NULL)
}

print_ <- function(x, ...) {
  ushapes <- unique(x$variable$shape)
  ord <- order(nchar(ushapes), decreasing = TRUE)
  cat(sprintf("Shapes (%i): \n", length(ushapes)))
  for (ishape in seq_along(ushapes)) {
    ii <- ord[ishape]
    cat(sprintf("[%s]", ushapes[ii]), ": ")
    cat(paste(x$variable$name[x$variable$shape == ushapes[ii]], collapse = ", "), "\n")
  }
  
}

# cf <- raadtools::cpolarfiles()$fullname[1]
## nc <- tydync(cf)
## print_(nc)

