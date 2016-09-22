#' @importFrom ncdf4 nc_open nc_close
.varnames <- function(x) {
  names(.ndims(x))
}
.ndims <- function(x) {
  nc <- nc_open(x)
  dims <- sapply(nc$var, "[[", "ndims")
  nc_close(nc)
  dims
}

.dimnames <- function(x, varname) {
  nc <- nc_open(x)
  names(nc$dim[nc$var[[varname]]$dimids])
}

varcoords <- function(x, varname) {
  x <- NetCDF(x)
   arrange_(inner_join(select_(inner_join(dplyr::filter(x$variable, .dots = list(~name == varname)), x$vardim), "id", "dimids"), x$dimension, c("dimids" = "id")), "dimids")$name
}

#' @importFrom ncdf4 ncatt_get
#' @importFrom stats setNames
ncatts <- function(x) {
  on.exit(nc_close(ncf))
  ncf <- ncdf4::nc_open(x)
  global <- as_data_frame(ncdf4::ncatt_get(ncf, 0))
  var <- setNames(vector('list', length(ncf$var)), names(ncf$var))
  childvar <- var
  #lapply(names(ncf$var), 
  #              function(vname) as_data_frame(ncatt_get(ncf, vname)))
  for (vname in names(ncf$var)) {
    aaa <- ncatt_get(ncf, vname)
    lts <- lengths(aaa)
    if (length(unique(lts)) > 1) {
      tabs <- lapply(split(aaa, lts), as_data_frame)
      var[[vname]] <- tabs[[1]]
      childvar[[vname]] <- tabs[-1]
    } 
    
  }
  ## childvar just a leftover for now
  ## drop all NULL
  list(global = global, var = var[!unlist(lapply(var, is.null))], childvar = childvar)
}


#' Information about a NetCDF file, in convenient form.
#'
#' @param x path to NetCDF file
#' @export
#' @importFrom ncdf4 nc_open
#' @importFrom dplyr as_data_frame bind_rows data_frame 
#' @examples 
#' rnc <- NetCDF(system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package= "rancid"))
#' rnc
NetCDF <- function(x) {
  nc <- ncdf4::nc_open(x)
  dims <- do.call(dplyr::bind_rows, lapply(nc$dim, function(x) dplyr::as_data_frame(x[!names(x) %in% c("dimvarid", "vals", "units", "calendar")])))
  unlimdims <- NULL
  if (any(dims$unlim)) unlimdims <- do.call(dplyr::bind_rows, lapply( nc$dim[dims$unlim], function(x) as_data_frame(x[names(x) %in% c("id", "units", "calendar")])))
  ## do we care that some dims are degenerate 1D?
  ##lapply(nc$dim, function(x) dim(x$vals))
  dimvals <- do.call(dplyr::bind_rows, lapply(nc$dim, function(x) dplyr::data_frame(id = rep(x$id, length(x$vals)), vals = x$vals)))
  ## the dimids are in the dims table above
  groups <- do.call(dplyr::bind_rows, lapply(nc$groups, function(x) dplyr::as_data_frame(x[!names(x) %in% "dimid"]))) #as_data_frame[x[!names(x) %in% "dimid"]]))
  ## leave the fqgn2Rindex for now
  file <- dplyr::as_data_frame(nc[!names(nc) %in% c("dim", "var", "groups", "fqgn2Rindex")])
  ## when we drop these, how do we track keeping them elsewhere?
  var <- do.call(dplyr::bind_rows, lapply(nc$var, function(x) dplyr::as_data_frame(x[!names(x) %in% c("chunksizes", "id", "dims", "dim", "varsize", "size", "dimids")])))
  var$id <- sapply(nc$var, function(x) x$id$id)
  vardim <- do.call(bind_rows, lapply(nc$var, function(x) data_frame(id = rep(x$id$id, length(x$dimids)), dimids = x$dimids)))
  ## read attributes, should be made optional (?) to avoid long read time
  atts <- ncatts(x)
  class(atts) <- c("NetCDF_attributes", "list")
  nc_close(nc)
  x <- list(dimension = dims, unlimdims = unlimdims, dimvals = dimvals, groups = groups, file = file, variable = var, 
            vardim = vardim, attribute = atts)
  class(x) <- c("NetCDF", "list")
  x
}
#' @importFrom utils head
longlistformat <- function(x, n = 8) {
  if (length(x) <= n) return(x)
  paste(paste(head(x, n), collapse = ", "),  "...",  length(x) - n, "more ...")
}
#' @export
print.NetCDF_attributes <- function(x, ...) {
  print("NetCDF attributes:")
  print("Global")
  print("\n")
  print(x$global)
  print("\n")
  print("Variable attributes:")
  print(sprintf("variable attributes: %s", longlistformat(names(x$var))))
}

#' NetCDF file description functions. 
#' @param x NetCDF metadata object
#' @param ... ignored
#' @export
vars <- function(x, ...) UseMethod("vars")

#' @rdname vars
#' @export
vars.NetCDF <- function(x, ...) {
  x$variable
}

#' @rdname vars
#' @export
dims <- function(x, ...) UseMethod("dims")

#' @rdname vars
#' @export
dims.NetCDF <- function(x, ...) {
  x$dimension
}

#' @rdname vars
#' @export
dimvars <- function(x, ...) UseMethod("dimvars")

#' @rdname vars
#' @export
#' @importFrom dplyr %>% arrange_ filter filter_  inner_join select select_
dimvars.NetCDF <- function(x, ...) {
  dmv <- (dims(x) %>% filter_("create_dimvar") %>% select_("name"))$name
  ndv <- length(dmv)
  ndims <- rep(0, ndv)
  data_frame(name = dmv, 
             ndims = ndims, natts = ndims) 
             ## todo, how much is create_dimvar ncdf4 only?
             # prec = rep("float", ndv), 
             # units = rep("", ndv), 
             # longname = units, group_index = 
             # 
}


#' @param varname name of variable to get atts of (not yet implemented)
#'
#' @rdname vars
#' @export
atts <- function(x, ...) {
  UseMethod("atts")
}


#' @rdname vars
#' @export
atts.NetCDF <- function(x, varname = "globalatts", ...) {
  if (varname == "globalatts") {
    x$attribute$global 
  } else {
    ## TODO, this needs thought given that childvar
    ## can be recursive and possible NULL
    stopifnot(varname %in% vars(x)$name)
    x$attribute$var[[varname]]
  }
}

#' @importFrom dplyr filter_
"[[.NetCDF" <- function(x,i,j,...,drop=TRUE) {
  var <-  filter_(x$variable, .dots = list(~name == i))
 class(var) <- c("NetCDFVariable", class(var))
  var
}

print.NetCDFVariable <- function(x, ...) {
  print(t(as.matrix(x)))
}

#library(lazyeval)
"[.NetCDFVariable" <- function(x, i, j, ..., drop = TRUE) {
  # il <- lazy(i)
  # jl <- lazy(j)
  # dl <- lazy(...)
  #  print(dl)
  #  print( format(dl$expr))
  dots <- list(...)
  #  print(dots)
  ## this is ok, but also need array[i] type indexing, as well as array[matrix]
  if (missing(i)) stop("argument i must be provided")
  
  if (missing(j) & x$ndims > 1L) stop("argument j must be provided")
  #browser()
  nindex <- length(dots) + as.integer(!missing(i)) + as.integer(!missing(j))
  #print(nindex)
  if (!nindex == x$ndims) stop(sprintf("number of index elements must match dimensions of variable: %i", x$ndims))
  #print(i)
  ## now the hard work, see nchelper
  args <- c(list(i), if (missing(j)) list() else list(j), dots)
  # largs <- format(il$expr)
  #return(largs)
  # print(format(il$expr))
  #if (!missing(j)) largs <- sprintf("%s,%s", largs, format(jl$expr))
  
  #if (!missing(...)) sprintf(largs, format(dl$expr))
  # print('after')
  args
  # sprintf("%s[")
}


# nc <- NetCDF("data/mer_his_1992_01.nc")
# Cs_w <- nc[["Cs_w"]]
# lon_u <- nc[["lon_u"]]
# Cs_w[2]
# lon_u[2,3]
#
#
