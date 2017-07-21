fs <- raadfiles::ncep2_uwnd_6hr_files()

print(fs$file[1])
library(tidync)
library(dplyr)
## get the full table for only the first time step
system.time({
  fnc <- tidync(fs$fullname[1])
 tab1 <- fnc %>% 
  hyper_filter(time = index == 1) %>% 
  hyper_tibble()
})
tab1

## now, get all the data from the entire file and flesh
## out the big table
system.time({
slabs <- hyper_slice(fnc)
})
str(slabs)

## here we are not getting the right time value, the only dimension that's varying in the loop
## but this gives and indication of something like the best timing we might get by shortcutting that
system.time({
  d <- bind_rows(lapply(seq_len(dim(slabs[[1]])[3]), function(i) {tab1 %>% mutate(uwnd = c(slabs[[1]][,,i]))}))
})
  
d
pryr::object_size(d)
## now, what if we save ourselves that work and do the full expansion for all 4 dimensions (level is degenerate but
## no big deal at least it generalizes)
system.time({
d1 <- hyper_tibble(fnc)
})

d1
d1 %>% distinct(time)
d1 %>% distinct(level)
d1 %>% distinct(lon)

pryr::object_size(d1)

file.info(fs$fullname[1])$size/1e6

## 300 hours for 100K 52Mb files
(1e5 * 11)/3600






## some independent comparisons

## traditional input
system.time({
  nc <- ncdf4::nc_open(fs$fullname[1])
  v1 <- ncdf4::ncvar_get(nc, "uwnd")
  ncdf4::nc_close(nc)
})

system.time({
  data.frame(uwnd = as.vector(v1))
})

system.time({
  tibble(uwnd = as.vector(v1))
})

system.time({
  nc <- ncdf4::nc_open(fs$fullname[1])
  v1 <- ncdf4::ncvar_get(nc, "uwnd")
  axes <- lapply(c("lon", "lat", "level", "time"), function(vname) ncdf4::ncvar_get(nc, vname))
  ncdf4::nc_close(nc)
})


arr_melt <- function(a, drop = FALSE, na.rm = FALSE, axes = NULL) {
  if (is.null(axes)) {
   axes <- lapply(dim(a), function(n) seq_len(n))
  }
  if (drop) {
    ones <- unlist(lapply(axes, length)) == 1L
    if (sum(ones) < 1) stop("no non-degenerate dimensions, use drop = FALSE")
    axes <- axes[!ones]
  }

  dims <- dim(a)
  total_prod <- prod(dims)
  nm <- c("row", "col", letters)[seq_along(axes)]

  v <- as.vector(a)

  keep <- rep_len(TRUE, length.out = total_prod)
  if (na.rm) keep <- !is.na(v)
  tib <- tibble::tibble(data = v)
  prod_dims <- 1
  for (i in seq_along(axes)) {
    nd <- dims[i]
    #print(system.time({
    ax <- axes[[i]]
    tib[[nm[i]]] <- rep(ax, each = prod_dims, length.out = total_prod) #[keep]
    #}))
    prod_dims <- prod_dims * nd
  }
  tib

}



arr_melt2 <- function(a, drop = FALSE, na.rm = FALSE, axes = NULL) {
  if (is.null(axes)) {
    axes <- lapply(dim(a), function(n) seq_len(n))
  }
  if (drop) {
    ones <- unlist(lapply(axes, length)) == 1L
    if (sum(ones) < 1) stop("no non-degenerate dimensions, use drop = FALSE")
    axes <- axes[!ones]
  }
  
  dims <- dim(a)
  total_prod <- prod(dims)
  nm <- c("row", "col", letters)[seq_along(axes)]
  
  v <- as.vector(a)
  
  keep <- rep_len(TRUE, length.out = total_prod)
  if (na.rm) keep <- !is.na(v)
#  tib <- tibble::tibble(data = v)
  tib <- do.call(tidyr::crossing, axes)
  tib[["data"]] <- v
  tib
  }

system.time({
  nc <- ncdf4::nc_open(fs$fullname[1])
  v1 <- ncdf4::ncvar_get(nc, "uwnd")
  axes <- lapply(c("lon", "lat", "level", "time"), function(vname) ncdf4::ncvar_get(nc, vname))
  axes <- setNames(axes, c("lon", "lat", "level", "time"))
  ncdf4::nc_close(nc)
  tib <- arr_melt(v1, axes = axes)
})
library(arrayhelpers)
system.time({
  nc <- ncdf4::nc_open(fs$fullname[1])
  v1 <- ncdf4::ncvar_get(nc, "uwnd")
  axes <- lapply(c("lon", "lat", "level", "time"), function(vname) ncdf4::ncvar_get(nc, vname))
  axes <- setNames(axes, c("lon", "lat", "level", "time"))
  ncdf4::nc_close(nc)
  #tib2 <- array2df(v1)
  #tib2 <- reshape2::melt(v1)
  tib2 <- arr_melt2(v1, axes = axes)
})
dim(tib2)
