
context("activate")

ufile <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
tidync(ufile)
## also make sure we can active a new grid, this is the dimension ref comma separated (no brackets)
tidync(ufile) %>% activate("D3,D12")


test_that("multiplication works", {
  tidync(ufile) %>% active() %>% expect_equal("D0,D12")
  
  ufile <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
  tidync(ufile)
  tidync(ufile, 3)
  ## this can't work without NSE handling
  #tidync(ufile, handbook5Id)
  tidync(ufile, "handbook5Id")
  tidync(ufile, "D8,D7")
  
  ## activate won't change it if not specified
  tidync(ufile, 4) %>% activate()
  tidync(ufile) %>% activate(handbook5Id)
  tidync(ufile)  %>% activate("handbook5Id")
  tidync(ufile)  %>% activate("D8,D7")
  
})
