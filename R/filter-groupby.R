## replace by velox/tabularaster??
## 
## ## @importFrom rlang .data
## ## @importFrom raster setValues
## fast_cellnumbers <- function(r, p) {
##   p <- sf::st_as_sf(p)
##   p$ID <- seq_len(nrow(p))
##   r <- raster::setValues(r, 0)
##   rr <- fasterize::fasterize(p, r, field = "ID")
##   tabularaster::as_tibble(rr) %>% dplyr::filter(!is.na(.data$cellvalue)) %>%
##    dplyr::rename(object_ = .data$cellvalue, cell_ = .data$cellindex)
## }
## 
## ## Hyper group by
## ## 
## ## Group the nominated space within a grid by an polygon object
## ## @param x a tidync object
## ## @param object a polygon object
## ## @param ns the dimensions of the active grid to use as the nominal space
## ## @importFrom dplyr between filter
## ## @importFrom raster extent xmin xmax ymin ymax
## ## @noRd
## hyper_group_by <- function(x, object, ns = 1:2) {
##   nominal_space <- active_axis_transforms(x)
##   ext <- try(raster::extent(object), silent = TRUE)
##   if (inherits(ext, "try-error") & "sf" %in% class(object)) {
##     ext <- raster::extent(attributes(object[[attr(object, "sf_column")]])[["bbox"]][c("xmin", "xmax", "ymin", "ymax")])
##   }
##   xynames <- names(nominal_space)[ns]
##   xs <- nominal_space[[1]][[xynames[1]]]
##   ys <- nominal_space[[2]][[xynames[2]]]
##   nominal_space[[1]] <- nominal_space[[1]] %>% dplyr::filter(between(xs, raster::xmin(ext), raster::xmax(ext))) 
##   nominal_space[[2]]  <- nominal_space[[2]] %>% dplyr::filter(between(ys, raster::ymin(ext), raster::ymax(ext)))
##   xs <- nominal_space[[1]][[xynames[1]]]
##   ys <- nominal_space[[2]][[xynames[2]]]
##   nominal_space <- tibble::as_tibble(expand.grid(x = xs, y = rev(ys)))
##   cn <- fast_cellnumbers(raster::rasterFromXYZ(nominal_space), object)
##   #ok <- rep(FALSE, nrow(nominal_space))
##   #ok[cn$cell_] <- TRUE
##   object <- rep(NA_integer_, nrow(nominal_space))
##   object[cn$cell_] <- cn$object_
##   nominal_space$object <- object
##   #attr(hf, "nominal_space") <- nominal_space
##   nominal_space 
## }
## 
