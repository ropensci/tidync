#' Print tidync data
#' 
#' Print method for the 'tidync_data' list of arrays returned by [hyper_array()]. 
#'
#' The output lists the variables and their dimensions of an object from a  
#' previous call to [tidync()], and possibly [hyper_filter()]. The available
#' data will differe from the source in terms of variables (via `select_var` in 
#' [hyper_array]) and the lengths of each dimension (via named expressions in
#' [hyper_filter()]). 
#' @param x 'tidync_data' object (from [hyper_array()])
#' @param ... reserved args
#' @seealso tidync_data
#' @return the input object invisibly
#' @export
#' @examples 
#' argofile <- system.file("extdata/argo/MD5903593_001.nc", package = "tidync")
#' argodata <- tidync(argofile) %>% hyper_filter(N_LEVELS = index < 5) %>% 
#'               hyper_array(select_var = c("TEMP_ADJUSTED", "PRES"))
#' print(argodata)
print.tidync_data <- function(x, ...) {
  cat("Tidync Data Arrays\n")
  cat(sprintf("Variables (%i): %s\n", length(x), paste(sprintf("'%s'", names(x)), collapse = ", ")))
  cat(sprintf("Dimension (%i): %s\n", length(dim(x[[1]])), paste(dim(x[[1]]), collapse = ", ")))
  dims <- attr(x, "transforms")
  dims <- lapply(dims, function(d) d %>% dplyr::filter(.data$selected))
  nams <- names(dims)
  dims<- do.call(rbind, 
          lapply(nams, function(a) {
            v <- dims[[a]][[a]]; 
            tibble::tibble(name = a, min = min(v), max = max(v))}
            ))
  cat(sprintf("Source: %s\n", attr(x, "source")$source[1L]))
  invisible(x)
}

# hyper_array.tidync_data <- function(x, ...) {
#   stop("no hyper_array() method for 'tidync_data', please reset with `tidync(x)`")
# }
#' @name tidync
#' @export
tidync.tidync_data <- function(x, what, ...) {
  tidync(x$source$source[1L], what = what, ...)
}