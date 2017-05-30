#' Array subset by nse
#'
#' NSE arguments must be named as per the dimensions in the variable. This is a restrictive variant of `dplyr::filter`, 
#' with a syntax more like `dplyr::mutate`. This ensures that each element is named, so we know which dimension to 
#' apply this to, but also that the expression evaluated against can do some extra work for a nuanced test. 
#' @param x NetCDF object
#' @param ... currently ignored
#'
#' @return data frame
#' @export
#' @importFrom purrr map
#' @importFrom dplyr group_by mutate summarize
#' @examples
#' ## inst/dev/filtrate_early.R
#' ##http://rpubs.com/cyclemumner/tidyslab
#' # 
hyper_filter <- function(x, ...) {
  UseMethod("hyper_filter")
}
#' @name hyper_filter
#' @export
#' @importFrom dplyr %>% mutate 
#' @importFrom forcats as_factor
hyper_filter.NetCDF <- function(x, ...) {
  
  dimvals <- dimension_values(x) #%>% 
  #dplyr::group_by(.data$name)
  dimvals$step <- unlist(lapply(split(dimvals, forcats::as_factor(dimvals$name)), function(x) seq_len(nrow(x))))
  trans <-  split(dimvals, forcats::as_factor(dimvals$name)) 
  
  ## hack attack
  for (i in seq_along(trans)) names(trans[[i]]) <- gsub("^vals$", trans[[i]]$name[1], names(trans[[i]]))
  
  quo_named <- rlang::quos(...)
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100'")
  quo_noname <- unname(quo_named)
  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    trans[[iname]] <- dplyr::filter(trans[[iname]], !!!quo_noname[i])
    
  }
  trans <- lapply(trans, function(ax) {ax$filename <- x$file$filename; ax})
  #trans
  ##trans %>% purrr::map(.f = function(x) x %>% dplyr::summarize(start = min(.data$step), count = dplyr::n()))
  #lapply(trans, function(x) tibble(name = x$name[1], start = min(x$step), count = length(x$step)))
   hyper_filter(trans) %>% activate(active(x))
  
}

#' @name hyper_filter
#' @export
hyper_filter.default <- function(x, ...) {
  structure(x, class = c("hyperfilter", class(x)))
}
#' @name hyper_filter
#' @export
hyper_filter.character <- function(x, ...) {
  NetCDF(x) %>% hyper_filter(...)
}
#' @name hyper_filter
#' @export
print.hyperfilter <- function(x, ...) {
  x <- bind_rows(lapply(x,  function(a) summarize_all(a %>% select(-filename, -.dimension_, -id, -step) %>% group_by(name), funs(min, max, length))))
  print("filtered dimension summary: ")
  print(x)
  invisible(x)
}


#' Activate
#'
#' Set the context for subsequent manipulations. 
#' 
#' `activate` puts the named variable first
#' `active` gets and sets the active variable
#' @param .data NetCDF object
#' @param what name of a variable
#' @return NetCDF object
#' @importFrom activate activate active active<- 
#' @export active activate active<- 
#' @rdname activate 
#' @aliases active activate active<- 
#' @examples
#' rnc <- NetCDF(system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump"))
#' activate(rnc, "palette")
#' @name activate
#' @export
activate <- function(.data, what) UseMethod("activate")
#' @name activate
#' @export
activate.NetCDF <- function(.data, what) {
  what_name <- deparse(substitute(what))
  if (what_name %in% var_names(.data)) what <- what_name
  active(.data) <- what
  .data
}
#' @name activate
#' @export
activate.hyperfilter <- function(.data, what) {
  what_name <- deparse(substitute(what))
 # if (what_name %in% var_names(.data)) what <- what_name
  active(.data) <- what
  .data
}
#' @param x NetCDF object
#' @name activate
#' @export
active.NetCDF <- function(x) {
  attr(x, 'active')
}
#' @name activate
#' @export
active.hyperfilter <- active.NetCDF
#' @param value name of variable to be active
#' @name activate
#' @export
`active<-.NetCDF` <- function(x, value) {
  vn <- var_names(x)
  if (!value %in% vn) {
    stop(sprintf('Only possible to activate existing variables: %s', paste(vn, collapse = ", ")), call. = FALSE)
  }
  attr(x, 'active') <- value
  x
}
#' @param value name of variable to be active
#' @name activate
#' @export
`active<-.hyperfilter` <- function(x, value) {

  attr(x, 'active') <- value
  x
}

#' hyper slab index
#' 
#' @param x
#' @param ... expressions to `filtrate`
#' @export
hyper_index <- function(x,  ...) {
  UseMethod("hyper_index")
}
#' @export
#' @name hyper_index
hyper_index.tbl_df <- function(x, ...) {
  structure(x, class = c("hyperslab", class(x)))
}
#' @export
#' @name hyper_index
hyper_index.NetCDF <- function(x, ...) {
  x %>% hyper_filter(...) %>% hyper_index()

}
#' @export
#' @name hyper_index
hyper_index.character <- function(x, varname, ...) {
  NetCDF(x) %>% activate(varname) %>%  hyper_index(...)
}
#' @export
#' @name hyper_index
hyper_index.hyperfilter <- function(x, ...) {
  bind_rows(lapply(x, 
                   function(sub_trans) tibble(name = sub_trans$name[1], 
                                              start = min(sub_trans$step), 
                                              count = length(sub_trans$step), 
                                              variable = active(x), 
                                              file = sub_trans$filename[1L]))) %>% hyper_index()
}

#' hyper slice
#' 
#' @param x a hyper slab
#' @param ... ignored
#' @export
hyper_slice <- function(x, ...) {
  UseMethod("hyper_slice")
}
#' @name hyper_slice
#' @export
hyper_slice.hyperslab <- function(x, ...) {
  ncdf4::ncvar_get(ncdf4::nc_open(x$file[1]), x$variable[1], 
                   start = x$start, count = x$count)
}
#' @name hyper_slice
#' @export
hyper_slice.NetCDF <- function(x, ...) {
  x %>% hyper_index(...) %>% hyper_slice()
}
#' @name hyper_slice
#' @export
hyper_slice.character <- function(x, ...) {
  NetCDF(x) %>% hyper_index(...) %>% hyper_slice()
}







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
#' rnc <- NetCDF(system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump"))
#' var_names(rnc)
var_names <- function(x, ...) {
  UseMethod("var_names")
}
#' @export
#' @name var_names
var_names.character <- function(x, ...) {
  var_names(NetCDF(x))
}
#' @export
#' @name var_names
var_names.NetCDF <- function(x, ...) {
  x$variable$name
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
#' rnc <- NetCDF(system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump"))
#' dim_names(rnc)
dim_names <- function(x, ...) {
  UseMethod("dim_names")
}
#' @export
#' @name dim_names
dim_names.character <- function(x, ...) {
  var_names(NetCDF(x))
}
#' @export
#' @name dim_names
dim_names.NetCDF <- function(x, ...) {
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
#' rnc <- NetCDF(system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "ncdump"))
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
#' @export

dimension_values.NetCDF <- function(x) {
  dimids <- x$variable[x$variable$name == active(x), ]
    #dplyr::select(.data$name, .data$.variable_) %>% 
  dimids <- dimids[, c("name", ".variable_")]
  dimids <- dimids %>%  dplyr::inner_join(x$vardim) %>% select(.data$.dimension_)
  
  dim_names <- x$dimension[, c("name", ".dimension_")]
  
  ## forcats means we maintain the right order
  dimids %>% #dplyr::transmute(id = dimids) %>%  
    dplyr::inner_join(x$dimension_values, ".dimension_") %>% 
    dplyr::inner_join(dim_names, ".dimension_")
  
  
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
#' @export
variable_dimensions <- function(x) {
  #aa <- x$variable %>% 
 aa <- x$variable[x$variable$name == active(x), ]
  #bb <- aa %>% 
  #  dplyr::transmute(variable_name = .data$name, .data$.variable_) 
  bb <- tibble(variable_name = aa$name, .variable_ = aa$.variable_)
  cc <- bb %>% 
    #dplyr::inner_join(x$vardim) %>% 
    dplyr::inner_join(x$vardim, ".variable_") %>% 
    inner_join(x$dimension, ".dimension_")
  dd <- tibble(variable_name = cc$variable_name, 
               .variable_ = cc$.variable_, 
               .dimension_ = cc$.dimension_, 
               dimension_name = cc$name)
dd
}

#' deprecated
#' @name deprecated ncdump
#' @export
filtrate <- function(...) .Deprecated("hyper_filter")




#importFrom dplyr filter
#importFrom rlang quos
# #filtrate(data.frame(d = 1:10), y = y > 2, z = z == 5, x = dplyr::between(x, 18, 22))
# filtrate0 <-  function(df, ...) {
#   trans <- list(y = data.frame(y = 1:10), 
#                 x = data.frame(x = 4:50), 
#                 z = data.frame(z = 1:19))
#   quo_named <- rlang::quos(...)
#   quo_noname <- unname(quo_named)
#   for (i in seq_along(quo_named)) {
#     iname <- names(quo_named)[i]
#     trans[[iname]] <- dplyr::filter(trans[[iname]], !!!quo_noname[i])
#   }
#  trans
# }




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
print.NetCDF <- function(x, ...) {
  form <- active(x)
  vn <- c(form, setdiff(var_names(x), form))
  if (length(vn)> 1) {
    form <- sprintf("%s, (%s)", vn[1L],  paste(vn[-1L], collapse = ", "))
  }
  cat(sprintf("Variables: %s", form), "\n")
  
  cat(sprintf("Dimensions: \n", ""))
  print(variable_dimensions(x) %>% 
          inner_join(dims(x) %>% dplyr::transmute(.data$.dimension_, dimension_length = .data$len)))
  # print(x$dimension %>% 
  #         dplyr::arrange(.data$id)  %>% 
  #         transmute(.data$name, length = .data$len, 
  #                   unlimited= .data$unlim ) %>% as.data.frame())
  invisible(NULL)
}

