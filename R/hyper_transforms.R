#' Axis transforms
#'
#' Axis 'transforms' are data frames of each dimension in a NetCDF source.
#' `hyper_transforms` returns a list of the active transforms by default, 
#'
#' Each transform is available by name from a list, and each data frame has the
#' coordinate of the dimension, its index, and a 'selected' variable set by the
#' filtering expressions in `hyper_filter` and used by the read-functions
#' `hyper_array` and `hyper_tibble`.
#'
#' Use `hyper_transforms` to interrogate and explore the available dimension
#' manually, or for development of custom functions.
#' @param x tidync object
#' @param all set to `TRUE` to return all transforms, not only active ones 
#' @param ... ignored
#' @export
#' @return list of axis transforms
#' @examples
#' l3file <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
#' f <- system.file("extdata", "oceandata", l3file, package = "tidync")
#' ax <- tidync(f) %>% hyper_transforms()
#' names(ax)
#' lapply(ax, dim)
#'
#' ## this function returns the transforms tidync knows about for this source
#' str(tidync(f)$transforms)
#' names(hyper_transforms(tidync(f), all = TRUE))
hyper_transforms <- function(x, all = FALSE, ...) {
  UseMethod("hyper_transforms")
}
axis_transforms <- function(x, ...) {
  .Defunct("hyper_transforms")
  UseMethod("hyper_transforms")
}
active_axis_transforms <- function(x, ...) {
  grid <- x$grid %>% tidyr::unnest()
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

#' @name hyper_transforms
#' @importFrom dplyr row_number
#' @export
hyper_transforms.default <- function(x, all = FALSE, ...) {
  if (!all) return(active_axis_transforms(x, ...))
  grid <- x$grid
  axis <- x$axis
  dimension <- x$dimension
  source <- x$source
  ## ignore activation, just do all
  #active_x <- active(x)
  dims <- axis %>% 
   # dplyr::filter(.data$grid == active_x) %>% 
    #dplyr::inner_join(axis, "variable") %>% 
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
    ## axis might have a column called "i"  
    ## https://github.com/hypertidy/tidync/issues/74
    id_value <- dims$dimension[i]
    dim_name <- dims$name[i]
    dim_coord <- dims$coord_dim[i]
    axis <- mutate(axis, 
                   index = row_number(), 
                   id = id_value, 
                   name = dim_name, 
                   coord_dim =  dim_coord, 
                   selected = TRUE)
    
    
    transforms[[i]] <- axis

  }
  

  transforms
  
}
