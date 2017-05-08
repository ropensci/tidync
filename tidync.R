
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
#' example(NetCDF)
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
#' example(NetCDF)
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


#' Activate variable
#'
#' Stolen from tidygraph, we need this
#' 
#' `activate` puts the named variable first
#' `nctive` gets and sets the active variable
#' @param .data NetCDF object
#' @param what name of a variable
#'
#' @return NetCDF object
#' @export
#'
#' @examples
#' example(NetCDF)
#' activate(rnc, "palette")
activate <- function(.data, what) {
  UseMethod('activate')
}
#' @export
#' @rdname activate
activate.NetCDF <- function(.data, what) {
  what_name <- deparse(substitute(what))
  if (what_name %in% var_names(.data)) what <- what_name
  nctive(.data) <- what
  .data
}
#' @param x NetCDF object
#'
#' @rdname activate
#' @export
nctive <- function(x) {
  attr(x, 'nctive')
}
`nctive<-` <- function(x, value) {
  vn <- var_names(x)
  if (!value %in% vn) {
    stop(sprintf('Only possible to activate existing variables: %s', paste(vn, collapse = ", ")), call. = FALSE)
  }
  attr(x, 'nctive') <- value
  x
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
#' example(NetCDF)
#' dimension_values(rnc)
dimension_values <- function(x) {
  UseMethod("dimension_values")
}
#' @rdname dimension_values
#' @export
dimension_valus.character <- function(x) {
  dimension_values(NetCDF(x))
}
#' @rdname dimension_values
#' @export

dimension_values.NetCDF <- function(x) {
  dimids <- x$variable[x$variable$name == nctive(x), ]
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
  #  dplyr::filter(name == nctive(x))
  aa <- x$variable[x$variable$name == nctive(x), ]
  #bb <- aa %>% 
  #  dplyr::transmute(variable_name = .data$name, .data$.variable_) 
  bb <- tibble(variable_name = aa$name, .variable = aa$.variabel_)
  cc <- bb %>% 
    #dplyr::inner_join(x$vardim) %>% 
    dplyr::inner_join(x$vardim, ".variable_") %>% 
    inner_join(x$dimension, ".dimension_")
  dd <- tibble(variable_name = cc$variable_name, 
               .variable_ = cc$.variable, 
               .dimension_ = cc$.dimension_, 
               dimension_name = cc$name)
dd
}

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
filtrate <- function(x, ...) {
  UseMethod("filtrate")
}
#' @export
#' @importFrom dplyr %>% mutate 
#' @importFrom forcats as_factor
filtrate.NetCDF <- function(x, ...) {

  dimvals <- dimension_values(x) #%>% 
    #dplyr::group_by(.data$name)
 dimvals$step <- unlist(lapply(split(dimvals, dimvals$name), function(x) seq_len(nrow(x))))
  trans <-  split(dimvals, forcats::as_factor(dimvals$name)) 
  
  ## hack attack
  for (i in seq_along(trans)) names(trans[[i]]) <- c(".dimension_", "id", trans[[i]]$name[1], "name", "step")

  quo_named <- rlang::quos(...)
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100'")
  quo_noname <- unname(quo_named)
  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    trans[[iname]] <- dplyr::filter(trans[[iname]], !!!quo_noname[i])
  }
  #trans
  ##trans %>% purrr::map(.f = function(x) x %>% dplyr::summarize(start = min(.data$step), count = dplyr::n()))
  #lapply(trans, function(x) tibble(name = x$name[1], start = min(x$step), count = length(x$step)))
  trans
}





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
  activ <- nctive(x)
  print(sprintf("Variable: %s", activ))
  vn <- setdiff(var_names(x), activ)
  if (length(vn)> 0) {
  print(sprintf("(%s)", paste(vn, collapse = ", ")))
  }
  print(sprintf("Dimension:", ""))
  print(x$dimension %>% 
          dplyr::arrange(.data$id)  %>% 
          transmute(.data$name, length = .data$len, 
                    unlimited= .data$unlim ) %>% as.data.frame())
  invisible(NULL)
}

