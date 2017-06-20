we_are_raady <- function() {
  fp <- getOption("default.datadir")
  stat <- FALSE
  if (!is.null(fp) & file.exists(file.path(fp, "data"))) stat <- TRUE
 stat
}