
fast_cellnumbers <- function(r, p) {
  p <- sf::st_as_sf(p) 
  p$ID <- seq_len(nrow(p))
  r <- setValues(r, 0)
  rr <- fasterize::fasterize(p, r, field = "ID")
  tabularaster::as_tibble(rr) %>% dplyr::filter(!is.na(cellvalue)) %>% 
    dplyr::rename(object_ = cellvalue, cell_ = cellindex)
}


hyper_group_by <- function(x, object, ns = NULL) {
  hf <- hyper_filter(x)
  ext <- extent(object)
  xynames <- names(hf)[1:2]
  xs <- hf[[1]][[xynames[1]]]
  ys <- hf[[2]][[xynames[2]]]
  hf[[1]] <- hf[[1]] %>% filter(between(xs, xmin(ext), xmax(ext))) 
  hf[[2]]  <- hf[[2]] %>% filter( between(ys, ymin(ext), ymax(ext)))
  xs <- hf[[1]][[xynames[1]]]
  ys <- hf[[2]][[xynames[2]]]
  hf$nominal_space <- as_tibble(expand.grid(x = xs, y = ys))
  cn <- fast_cellnumbers(raster::rasterFromXYZ(hf$nominal_space), object)
  ok <- rep(FALSE, nrow(hf$nominal_space))
  ok[cn$cell_] <- TRUE
  hf$nominal_space$ok <- ok
  hf 
}

