library(tidync)
cf <- raadtools::cpolarfiles()$fullname[1]
nc <- tidync(cf)


shapes <- function(x) {
  shape_instances_byvar <- x$vardim %>% #dplyr::filter(!is.na(.dimension_)) %>%
    dplyr::group_by(.variable_) %>%
    split(.$.variable_) %>%
    purrr::map(function(xa) xa$.dimension_)

  shape_classify_byvar <- factor(unlist(lapply(shape_instances_byvar, function(xb) paste(xb, collapse = "-"))))
  levels(shape_classify_byvar)[shape_classify_byvar]
}

nc$variable$shape <- shapes(nc)
