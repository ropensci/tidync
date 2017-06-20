#' Variable names
#' 
#' 
#' @param x NetCDF object
#' @param ... currently ignored
#'
#' @return names of variables available
#' @export
#'
#' @examples
#' l3file <- "S2008001.L3m_DAY_CHL_chlor_a_9km.nc"
#' rnc <- tidync(system.file("extdata", l3file, package= "ncdump"))
#' var_names(rnc)
var_names <- function(x, ...) {
  UseMethod("var_names")
}
#' @export
#' @name var_names
var_names.character <- function(x, ...) {
  var_names(tidync(x))
}
#' @export
#' @name var_names
var_names.tidync <- function(x, ...) {
  x$shape$variable
}

#' Dimension names
#' 
#' 
#' @param x NetCDF object
#' @param ... ignored
#'
#' @return names of available dimensions 
#' @export
#'
#' @examples
#' l3file <- "S2008001.L3m_DAY_CHL_chlor_a_9km.nc"
#' rnc <- tidync(system.file("extdata", l3file, package= "ncdump"))
#' dim_names(rnc)
dim_names <- function(x, ...) {
  UseMethod("dim_names")
}
#' @export
#' @name dim_names
dim_names.character <- function(x, ...) {
  var_names(tidync(x))
}
#' @export
#' @name dim_names
dim_names.tidync <- function(x, ...) {
  x$variable$name
}



#' Dimension values
#' 
#' The dimension values are the coordinates, the positions along each axis. 
#'
#' @param x NetCDF object
#'
#' @return data frame of dimensions and their values
#' @export
#'
#' @examples
#' l3file <- "S2008001.L3m_DAY_CHL_chlor_a_9km.nc"
#' rnc <- tidync(system.file("extdata", l3file, package= "ncdump"))
#' dimension_values(rnc)
dimension_values <- function(x) {
  UseMethod("dimension_values")
}
#' @rdname dimension_values
#' @export
dimension_values.character <- function(x) {
  dimension_values(NetCDF(x))
}
#' @rdname dimension_values
#' @importFrom rlang .data
#' @export
dimension_values.tidync <- function(x) {
  dimids <- x$grid[x$grid$grid == active(x), ] %>% inner_join(x$variable, c("variable" = "name"))
  #dplyr::select(.data$name, .data$.variable_) %>% 
  dimids <- dimids %>% inner_join(x$dimension,c("dimids" = "id")) %>% 
    arrange(dimids)
  
  purrr::map(setNames(purrr::map(dimids$name, ~nc_get(x$file$dsn, .)), dimids$name), as_tibble)
  
  dimids <- dimids %>%  dplyr::inner_join(x$vardim, ".variable_") %>% 
    dplyr::select(.data$.dimension_)
  
  dim_names <- x$dimension[, c("name", ".dimension_")]
  
  ## forcats means we maintain the right order
  dimids %>% #dplyr::transmute(id = dimids) %>%  
    dplyr::inner_join(x$dimension_values, ".dimension_") %>% 
    dplyr::inner_join(dim_names, ".dimension_")
  
  
}

grid_dimension <- function(x) UseMethod("grid_dimension")
grid_dimension.tidync <- function(x) {
  x$grid %>% dplyr::filter(grid == active(x)) %>% 
    inner_join(x$variable, c("variable" = "name"))
}
#' Dimensions of a variable
#'
#' @param x NetCDF object
#'
#' @export
variable_dimensions <- function(x) {
  UseMethod("variable_dimensions")
}
#' @rdname variable_dimensions
#' @export
variable_dimensions <- function(x) {
  variable_dimensions(NetCDF(x))
}
#' @rdname variable_dimensions
#' @importFrom dplyr inner_join
#' @export
variable_dimensions <- function(x) {
  #aa <- x$variable %>% 
  aa <- x$variable[x$variable$name == active(x), ]
  #bb <- aa %>% 
  #  dplyr::transmute(variable_name = .data$name, .data$.variable_) 
  bb <- tibble::tibble(variable_name = aa$name, .variable_ = aa$.variable_)
  cc <- bb %>% 
    dplyr::inner_join(x$vardim, ".variable_") %>% 
    dplyr::inner_join(x$dimension, ".dimension_")
  dd <- tibble::tibble(variable_name = cc$variable_name, 
                       .variable_ = cc$.variable_, 
                       .dimension_ = cc$.dimension_, 
                       dimension_name = cc$name)
  dd
}

