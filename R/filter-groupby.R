

hyper_group_by <- function(x, object) {
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
  object <- as(object, "Spatial")
  ## wow, sp over is very slow here, not sure why
#  cn <- sp::over(SpatialPoints(as.matrix(hf$nominal_space), proj4string = CRS(proj4string(object))), 
#                 sp::geometry(object))
 r <- rasterFromXYZ(hf$nominal_space)
  tb <- as_tibble(r, value = FALSE)
  tb$ok <- FALSE
  tb$ok[cellnumbers(r, object)$cell_] <- TRUE
  hf$nominal_space$ok <- tb$ok #!is.na(cn)
  hf 
}

