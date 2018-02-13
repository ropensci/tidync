## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(pillar.subtle = FALSE, pillar.sigfig = 4)

## ------------------------------------------------------------------------
f <- system.file("extdata", "ifremer", "20171002.nc", package = "tidync")
library(RNetCDF)
print.nc(open.nc(f))

## ------------------------------------------------------------------------
library(tidync)
tidync(f)


## ------------------------------------------------------------------------
concentration <- tidync(f) %>% activate(concentration) 

concentration %>% hyper_filter() 

## ------------------------------------------------------------------------

concentration %>% hyper_filter(nj = nj < 20)



## ------------------------------------------------------------------------
concentration %>% hyper_filter(ni = index < 20, nj = dplyr::between(index, 30, 100))


## ------------------------------------------------------------------------
hf <- concentration %>% hyper_filter(ni = index < 20, nj = dplyr::between(index, 30, 100))

## as an array
arr <- hf %>% hyper_slice()
str(arr)

## as a data frame

#concentration %>% hyper_tibble() %>% filter(!is.na(concentration))


