
#' Variable names
#' 
#' 
#' @param x 
#' @param ... 
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
#' @param x 
#' @param ... 
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
#' @param x 
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
  dimids <- x$variable %>% filter(name == nctive(x)) %>% 
    dplyr::select(name, .data$.variable_) %>% 
    dplyr::inner_join(x$vardim) %>% select(.data$.dimension_)
  ## forcats means we maintain the right order
  dimids %>% #dplyr::transmute(id = dimids) %>%  
    dplyr::inner_join(x$dimension_values) %>% 
    dplyr::inner_join(x$dimension %>% 
                        dplyr::select(.data$id, .data$name))  
  
}

#' Dimensions of a variable
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
  dimids <- x$variable %>% 
    dplyr::filter(name == nctive(x)) %>% 
    dplyr::select(name, .data$.variable_) %>% 
    #dplyr::inner_join(x$vardim) %>% 
    dplyr::inner_join(x$vardim, ".variable_") %>% 
    inner_join(x$dimension, ".dimension_")
}

#' Array subset by nse
#'
#' NSE arguments must be named as per the dimensions in the variable. This is a restrictive variant of `dplyr::filter`, 
#' with a syntax more like `dplyr::mutate`. This ensures that each element is named, so we know which dimension to 
#' apply this to, but also that the expression evaluated against can do some extra work for a nuanced test. 
#' @param x 
#' @param ... 
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

  dimvals <- dimension_values(x) %>% dplyr::group_by(name) %>% dplyr::mutate(step = row_number())
  trans <-  dimvals %>% split(forcats::as_factor(.$name)) 
  
  ## hack attack
  for (i in seq_along(trans)) names(trans[[i]]) <- c(".dimension_", "id", trans[[i]]$name[1], "name", "step")

  quo_named <- rlang::quos(...)
  if (any(nchar(names(quo_named)) < 1)) stop("subexpressions must be in 'mutate' form, i.e. 'lon = lon > 100'")
  quo_noname <- unname(quo_named)
  for (i in seq_along(quo_named)) {
    iname <- names(quo_named)[i]
    trans[[iname]] <- dplyr::filter(trans[[iname]], !!!quo_noname[i])
  }
  trans %>% purrr::map(.f = function(x) x %>% dplyr::summarize(start = min(step), count = n()))
  
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





#' @rdname activate
#' @export
#' @importFrom dplyr %>% arrange transmute
#' @examples 
#' f <- "/rdsi/PRIVATE/raad/data/eclipse.ncdc.noaa.gov/pub/OI-daily-v2/NetCDF/2017/AVHRR/avhrr-only-v2.20170502_preliminary.nc"
#' x <- NetCDF(f)
#' nctive(x)
#' x <- activate(x, "sst") 
print.NetCDF <- function(x, ...) {
  activ <- nctive(x)
  print(sprintf("Variable: %s", activ))
  vn <- setdiff(var_names(x), activ)
  if (length(vn)> 0) {
  print(sprintf("(%s)", paste(vn, collapse = ", ")))
  }
  print(sprintf("Dimension:", ""))
  print(x$dimension %>% dplyr::arrange(id)  %>% transmute(name, length = len, unlimited= unlim ) %>% as.data.frame())
  invisible(NULL)
}

