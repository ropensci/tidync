
# shapes <- function(x) {
#   shape_instances_byvar <- x$vardim %>% #dplyr::filter(!is.na(.dimension_)) %>%
#     dplyr::group_by(.variable_) %>%
#     split(.$.variable_) %>%
#     purrr::map(function(xa) xa$.dimension_)
#   
#   shape_classify_byvar <- factor(unlist(lapply(shape_instances_byvar, function(xb) paste(xb, collapse = "-"))))
#   levels(shape_classify_byvar)[shape_classify_byvar]
# }

shapes <- function(x) {
  shape_instances_byvar <- x$variable %>% group_by(name) %>% 
    split(.$name) %>% purrr::map(function(xa) xa$dimids)
  shape_classify_byvar <- factor(unlist(lapply(shape_instances_byvar, function(xb) paste(xb, collapse = "-"))))
  tibble(variable  = names(shape_classify_byvar), shape = levels(shape_classify_byvar)[shape_classify_byvar])
}