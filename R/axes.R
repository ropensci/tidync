#' Axis transforms
#'
#' @param x tidync object
#' @param ... ignored
#'
#' @return list of axis transforms
axis_transforms <- function(x, ...) {
  UseMethod("axis_transforms")
}
#' @name axis_transforms
active_axis_transforms <- function(x, ...) {
  grid <- x$grid
  axis <- x$axis
  dimension <- x$dimension
  active_x <- active(x)
  dims <- grid %>% 
    dplyr::filter(.data$grid == active_x) %>% 
    dplyr::inner_join(axis, "variable") %>% 
    dplyr::inner_join(dimension, c("dimension" = "id")) %>% 
    dplyr::distinct(.data$name, .data$dimension,  .keep_all = TRUE) %>%  
    dplyr::select(.data$name, .data$dimension, .data$length, .data$coord_dim)
  x$transforms[dims$name]
}

#' @name axis_transforms
#' @importFrom dplyr row_number
axis_transforms.default <- function(x, ...) {
  grid <- x$grid
  axis <- x$axis
  dimension <- x$dimension
  source <- x$source
  ## ignore activation, just do all
  #active_x <- active(x)
  dims <- grid %>% 
   # dplyr::filter(.data$grid == active_x) %>% 
    dplyr::inner_join(axis, "variable") %>% 
    dplyr::inner_join(dimension, c("dimension" = "id")) %>% 
    dplyr::distinct(.data$name, .data$dimension,  .keep_all = TRUE) %>%  
    dplyr::select(.data$name, .data$dimension, .data$length, .data$coord_dim)
  
  transforms <- vector("list", nrow(dims))
  names(transforms) <- dims$name
  
  for (i in seq_along(transforms)) {
    ll <- list(value = ifelse(rep(dims$coord_dim[i], dims$length[i]), 
                        nc_get(source$source, dims$name[i]), seq_len(dims$length[i])))
    axis <- tibble::as_tibble(ll)
    names(axis)  <- dims$name[i]
    axis <- mutate(axis, 
                   index = row_number(), 
                   id = dims$dimension[i], 
                   name = dims$name[i], 
                   coord_dim =  dims$coord_dim[i], 
                   selected = TRUE)
    
    ## add the "unit time" to the axis, see ncdf4.helpers for ways of finding out
    ## which dims are "time" 
    ## TODO: this also needs to avoid applying Gregorian assumptions, again ncdf4.helpers
    ## and futureheatwaves have some prior art
    if (dims$name[i] == "time") {
      meta <- ncmeta::nc_meta(source$source[1])
      attmeta <- try(tidyr::unnest(dplyr::filter(meta$attribute, variable == "time")), silent = TRUE)
      if (inherits(attmeta, "try-error")) {
        warning("unable to convert unit-based time")
      } else {
       ut <- RNetCDF::utcal.nc(dplyr::pull(dplyr::filter(attmeta, attribute == "units"), "value"), 
                              axis[["time"]], type = "c")
       axis[["unit_time"]] <- ut
      }
    }
    transforms[[i]] <- axis

  }
  

  transforms
  
}