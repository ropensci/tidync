tidync_default_options <- function() {
  list(tidync.large.data.check = TRUE, tidync.silent = FALSE)
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  rdo <- tidync_default_options()
  toset <- !(names(rdo) %in% names(op))
  if (any(toset)) options(rdo[toset])
  
  invisible()
}