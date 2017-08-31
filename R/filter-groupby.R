
#' @importFrom rlang .data
#' @importFrom raster setValues
fast_cellnumbers <- function(r, p) {
  #p <- sf::st_as_sf(p) 
  #p$ID <- seq_len(nrow(p))
  #r <- raster::setValues(r, 0)
  #rr <- fasterize::fasterize(p, r, field = "ID")
  #tabularaster::as_tibble(rr) %>% dplyr::filter(!is.na(cellvalue)) %>% 
  #  dplyr::rename(object_ = .data$cellvalue, cell_ = .data$cellindex)
}

#' Hyper group by
#' 
#' Group the nominated space within a grid by an polygon object
#' @param x a tidync object
#' @param object a polygon object
#' @param ns the dimensions of the active grid to use as the nominal space
#' @importFrom dplyr between filter
#' @importFrom raster extent xmin xmax ymin ymax
#' @noRd
hyper_group_by <- function(x, object, ns = 1:2) {
  hf <- hyper_filter(x)
  if (!is.null(attr(hf, "nominal_space"))) stop("object already has a nominated space!")
  ext <- raster::extent(object)
  xynames <- names(hf)[ns]
  xs <- hf[[1]][[xynames[1]]]
  ys <- hf[[2]][[xynames[2]]]
  hf[[1]] <- hf[[1]] %>% dplyr::filter(between(xs, xmin(ext), xmax(ext))) 
  hf[[2]]  <- hf[[2]] %>% dplyr::filter( between(ys, ymin(ext), ymax(ext)))
  xs <- hf[[1]][[xynames[1]]]
  ys <- hf[[2]][[xynames[2]]]
  nominal_space <- tibble::as_tibble(expand.grid(x = xs, y = ys))
  cn <- fast_cellnumbers(raster::rasterFromXYZ(nominal_space), object)
  ok <- rep(FALSE, nrow(nominal_space))
  ok[cn$cell_] <- TRUE
  nominal_space$ok <- ok
  attr(hf, "nominal_space") <- nominal_space
  
  hf 
}

