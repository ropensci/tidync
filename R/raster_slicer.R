#' Construct raster from slices
#' 
#' Experimental, internal
#' @keywords internal
#' @noRd
#' @examples
#' files <- raadtools::ghrsstfiles() 
#' tnc <- tidync(files$fullname[1])
#' library(dplyr)
#' system.time(r0 <- raster0(tnc, lon = between(lon, 138, 150), lat = between(lat, -50, -40), select_var = "analysis_error"))
#' library(raster)
#' plot(r0)
#' 
#' 
#' ha <- hyper_array(tnc, lon = between(lon, 138, 150), lat = between(lat, -50, -40), select_var = "analysis_error")
#' 
#' raster1(tnc, lon = between(lon, 138, 150), lat = between(lat, -50, -40), select_var = "analysis_error")
#' tnc %>% hyper_filter(lon = between(lon, 138, 150), lat = between(lat, -50, -40)) %>% 
#'   raster(select_var = "analysed_sst")
#' 
#' @name raster-tidync
set_ext0 <- function(x) {
  ## set index extent on a raster
  raster::setExtent(x, raster::extent(0, ncol(x), 0, nrow(x)))
}
#' @name raster-tidync
slicer <- function(x, slice = 1) {
  ## slice out the right matrix from a hyper_array list
  ## for 2D or 3D only currently
  dm <- dim(x)
  offs <- 0
  if (length(dm) > 2) offs <- (slice - 1) * prod(dm[1:2])
  
  rflip(matrix(x[seq_len(prod(dm[1:2])) + offs], dm))
}
#' @name raster-tidync
rflip <- function(x) {
  ## orient matrix to raster
  t(x[, ncol(x):1])
}
#' @name raster-tidync
raster0 <- function(x, ..., slice = 1) {
  if (!requireNamespace("raster", quietly = TRUE))
    stop("package raster required, try installing with 'install.packages(\"raster\")'")
  
  ## convert to index raster
  UseMethod("raster0") 
}
#' @name raster-tidync
raster0.default <- function(x, ..., slice = 1) {
  
  set_ext0(raster::raster(slicer(x[[1]], slice)))
}
#' @name raster-tidync
raster0.tidync <- function(x, ..., slice = 1) {
  raster0(hyper_array(x, ...), slice = slice)
}


#' @name raster-tidync
array_transforms <- function(x) {
  #' get hyper array transforms (could be axis_transforms)
  attr(x, "transforms")
}

#' @name raster-tidync
select_transforms <- function(x) {
  # only selected coordinates
  lapply(array_transforms(x), function(a) dplyr::filter(a, .data$selected))
}
#' @name raster-tidync
raster1 <- function(x, ..., slice = 1) {
  if (!requireNamespace("raster", quietly = TRUE))
    stop("package raster required, try installing with 'install.packages(\"raster\")'")
  
  ## raster with reference extent
  ha <- hyper_array(x, ...)
  tr <- select_transforms(ha)
  r <- raster0(ha, ..., slice = slice)
  dnames <- names(tr)
  
  ## need to detect reversals here
  xs <- tr[[dnames[1]]][[dnames[1]]]
  ys <- tr[[dnames[2]]][[dnames[2]]]
  dx <- diff(xs[1:2])
  dy <- diff(ys[1:2])

  ex <- raster::extent(min(xs), max(xs), min(ys), max(ys)) + c(dx, dy)/2
  raster::setExtent(r, ex)
  
}

#setOldClass("tidync")
#if (!isGeneric("raster"))
#  setGeneric("raster", function(x, ...)
#    standardGeneric("raster"))
#setMethod("raster", "tidync", raster1)
