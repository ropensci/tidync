#' Dplyr verbs for tidync
#' 
#' Very experimental
#' @param x tidync object
#' @param .data tidync object
#' @param ... passed into dplyr engine
#' @param add add to existing groups
#' @param shape experimental tidync
#' @rdname dplyr-verbs
#' @examples 
#' \dontrun{
#' #x <- tidync(raadtools::sshfiles()$fullname[1])
#' #  x %>% hyper_filter(longitude = longitude > 147, latitude = latitude < - 30) %>% group_by(longitude) %>%   summarize(adt = mean(adt, na.rm = TRUE))
#' #  x %>% hyper_filter(longitude = longitude > 147, latitude = latitude < - 30) %>% group_by(longitude) %>%   summarize_all(mean, na.rm = TRUE)
#' }
tbl_vars.tidync <- function(x) {
  c(x$variable$name[x$variable$active], 
    x$dimension$name[x$dimension$active])
}
groups.tidync <- function(x) {
  x$groups
}
#' @rdname dplyr-verbs
summarise.tidync <- function(.data, ...) {
  if (inherits(.data$groups, "sf")) {
    ## 1. create a raster (transform sf if needed)
    
    ns <- 1:2
    ax <- active_axis_transforms(.data)[ns]
    X <- ax[[1]][[names(ax)[1]]][ax[[1]][["selected"]]]
    Y <- ax[[2]][[names(ax)[2]]][ax[[2]][["selected"]]]
    ## 1a. obtain nominal sf axes from group_by  TODO ^^
    ex <- c(range(X) + c(-1, 1) * diff(X[1:2])/2, 
            range(Y) + c(-1, 1) * diff(Y[1:2])/2)
    
    r <- raster::raster(raster::extent(ex), 
                   ncols = sum(ax[[1]][["selected"]]), nrows = sum(ax[[2]][["selected"]]))
    
    ## 2. fasterize groups to it
    .data$groups$ID <- 1:nrow(.data$groups)
    #rcell <- fasterize::fasterize(.data$groups, r, "ID")
    cells <- tabularaster::cellnumbers(r, .data$groups)
    cells <- dplyr::filter(cells, !is.na(.data$cell_))
    ht <- hyper_tibble(.data, na.rm = FALSE)
  #  browser()
    ht[["tidync_group_"]] <- NA
    
    ht[["tidync_cell_"]] <- raster::cellFromXY(r, as.matrix(ht[names(ax)]))
    
    ht[["tidync_group_"]][match(cells$cell_, ht$tidync_cell_)] <- cells$object_
    ## 3. run actual group_by with sf-ID
            ## danger, see the flip here ////!!!
    #ht[["tidync_group_"]] <- values(flip(rcell, "y"))
    ## could use setdiff here with cellnumbers ...
    ht <- dplyr::filter(ht, !is.na(.data$tidync_group_))
   return( ht %>% group_by(.data$tidync_group_) %>% summarise(...))
  }
  hyper_tibble(.data) %>% group_by(!!!.data$groups) %>% summarise(...)
}
#' @importFrom dplyr group_by ungroup summarise
#' @rdname dplyr-verbs
group_by.tidync <- function(.x, ..., add = FALSE, shape = NULL) {
  if (add) stop('groupings cannot be added to')
  groups <- rlang::quos(...)
  x$groupshape <- FALSE
  if(!is.null(shape)) {
    if (length(groups) > 0) warning("'shape' is set, so bare grouping names ignored")
    groups <- shape
    x$groupshape <- TRUE
  }  
  .x$groups <- groups
  .x
}
#' @rdname dplyr-verbs
ungroup.tidync <- function(x, ...) {
  x$groups <- NULL
  x
}
## replace by velox/tabularaster??
## 
## ## @importFrom rlang .data
## ## @importFrom raster setValues
## fast_cellnumbers <- function(r, p) {
##   p <- sf::st_as_sf(p)
##   p$ID <- seq_len(nrow(p))
##   r <- raster::setValues(r, 0)
##   rr <- fasterize::fasterize(p, r, field = "ID")
##   tabularaster::as_tibble(rr) %>% dplyr::filter(!is.na(.data$cellvalue)) %>%
##    dplyr::rename(object_ = .data$cellvalue, cell_ = .data$cellindex)
## }
## 
## ## Hyper group by
## ## 
## ## Group the nominated space within a grid by an polygon object
## ## @param x a tidync object
## ## @param object a polygon object
## ## @param ns the dimensions of the active grid to use as the nominal space
## ## @importFrom dplyr between filter
## ## @importFrom raster extent xmin xmax ymin ymax
## ## @noRd
## hyper_group_by <- function(x, object, ns = 1:2) {
##   nominal_space <- active_axis_transforms(x)
##   ext <- try(raster::extent(object), silent = TRUE)
##   if (inherits(ext, "try-error") & "sf" %in% class(object)) {
##     ext <- raster::extent(attributes(object[[attr(object, "sf_column")]])[["bbox"]][c("xmin", "xmax", "ymin", "ymax")])
##   }
##   xynames <- names(nominal_space)[ns]
##   xs <- nominal_space[[1]][[xynames[1]]]
##   ys <- nominal_space[[2]][[xynames[2]]]
##   nominal_space[[1]] <- nominal_space[[1]] %>% dplyr::filter(between(xs, raster::xmin(ext), raster::xmax(ext))) 
##   nominal_space[[2]]  <- nominal_space[[2]] %>% dplyr::filter(between(ys, raster::ymin(ext), raster::ymax(ext)))
##   xs <- nominal_space[[1]][[xynames[1]]]
##   ys <- nominal_space[[2]][[xynames[2]]]
##   nominal_space <- tibble::as_tibble(expand.grid(x = xs, y = rev(ys)))
##   cn <- fast_cellnumbers(raster::rasterFromXYZ(nominal_space), object)
##   #ok <- rep(FALSE, nrow(nominal_space))
##   #ok[cn$cell_] <- TRUE
##   object <- rep(NA_integer_, nrow(nominal_space))
##   object[cn$cell_] <- cn$object_
##   nominal_space$object <- object
##   #attr(hf, "nominal_space") <- nominal_space
##   nominal_space 
## }
## 

