#' Tidy file collection
#'
#' @param x raadfiles, data frame with "fullname", or character vector
#' @param all scan metadata from all files?
#' @param ...  ignored
#'
#' @return data frame of filename "fullname" and "meta" in a list-col
#' @export
#'
#' @examples
#' #library(dplyr)
#' #files <- raadfiles::oisst_daily_files() %>% dplyr::filter(as.Date(date) > (Sys.Date() - 40))
#' #f <- tidyfile(files, all = FALSE)
tidyfile <- function(x, all = TRUE, ...){
  UseMethod("tidyfile")
}
#' @name tidyfile
#' @export
tidyfile.data.frame <- function(x, all = TRUE, ...) {
  if (is.null(x$fullname)) stop("x must have at least column 'fullname'")
  if (all(file.exists(x$fullname)) ) {
    class(x ) <- c("raadfiles", class(x))
  }
  tidyfile(x, all = all)
}
#' @name tidyfile
#' @export
tidyfile.raadfiles <- function(x, all = TRUE, ...) {
  if (all) {
    metalist <- tidyfile(x$fullname)
  } else {
    ml <- tidyfile(x$fullname[1L])
    metalist <- replicate(nrow(x), ml)
  }
  structure(dplyr::mutate(x, fullname = x$fullname, 
                 meta = metalist), class = c("tidyfile", "tbl_df", "tbl", "data.frame"))
}
#' @name tidyfile
#' @export
tidyfile.character <- function(x, all = TRUE, ...) {
  purrr::map(x, ncmeta::nc_meta)  
}

