# # Tidy file collection
# #
# # Tidy files include a trick to only store the full metadata of the
# # first file in the collection. This is extracted using `ncmeta::nc_meta` in
# # a form that may changed substantially. The current plan is to always
# # have an *estimable* set of metadata using the first file as a sample, 
# # but to be able to scan the full metadata as required. 
# # This is based on the model where a set of files *do match* or can be expected
# # to match in every shape other than the *unlimited* dimension, the one 
# # that is dispersed over files (usually "time"). 
# # @param x raadfiles, data frame with "fullname", or character vector
# # @param all scan metadata from all files?
# # @param ...  ignored
# #
# # @return data frame of filename "fullname" and "meta" in a list-col
# # @noRd
# #
# # @examples
# # #library(dplyr)
# # #files <- raadfiles::oisst_daily_files() %>% dplyr::filter(as.Date(date) > (Sys.Date() - 40))
# # #f <- tidyfile(files, all = FALSE)
# tidyfile <- function(x, all = FALSE, ...){
#   UseMethod("tidyfile")
# }
# # @name tidyfile
# # @noRd
# tidyfile.data.frame <- function(x, all = FALSE, ...) {
#   if (is.null(x$fullname)) stop("x must have at least column 'fullname'")
#   if (all(file.exists(x$fullname)) ) {
#     class(x ) <- c("raadfiles", class(x))
#   }
#   tidyfile(x, all = all)
# }
# # @name tidyfile
# # @noRd
# tidyfile.raadfiles <- function(x, all = FALSE, ...) {
#   if (all) {
#     metalist <- tidyfile(x$fullname)
#   } else {
#     ml <- tidyfile(x$fullname[1L])
#     metalist <- c(ml, vector("list", nrow(x) - 1L))
#   }
#   structure(dplyr::mutate(x, fullname = x$fullname, 
#                  meta = metalist), class = c("tidyfile", "tbl_df", "tbl", "data.frame"))
# }
# # @name tidyfile
# # @noRd
# tidyfile.character <- function(x, all = FALSE, ...) {
#   purrr::map(x, ncmeta::nc_meta)  
# }
# 
# 
# # 
# # # @name tidync
# # # @noRd
# # tidync.tidyfile <- function(x, ...) {
# #   meta <- ncmeta::nc_meta(x$fullname[1L])
# #   out <- structure(list(file = x, 
# #                         shape = shapes(meta), 
# #                         dimension = dimensions(meta)),
# #                    
# #                    class = "tidync")
# #   activate(out, shapes(meta)$shape[1])
# # }
# # # @name tidync
# # # @noRd
# # tidync.data.frame <- function(x, ...) {
# #   tidync(tidyfile(x))
# # }
# 
# 
