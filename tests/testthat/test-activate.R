
 context("activate")

 test_that("generic meaningless activation works", {
   l <- list(a = 1, b = 2)
   expect_warning(l <- activate(l, "b"))
   expect_warning(active(l) <- "b")
 }
           )
 test_that("various access and activation works", {

   ufile <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
   tnc <- tidync(ufile)

  ## changed with nesting of variables in nc_grids https://github.com/hypertidy/ncmeta/issues/26
  #expect_equal(active(tidync(ufile)) , "D0,D12")
  expect_equal(active(tidync(ufile)) , "D5,D12")
  ## also make sure we can active a new grid, this is the dimension ref comma separated (no brackets)
  #  tidync(ufile) %>% activate("D3,D12")
  #tidync(ufile, 3)
  ## this can't work without NSE handling
  #tidync(ufile, handbook5Id)
#  tidync(ufile, "handbook5Id")
#  tidync(ufile, "D8,D7")

  ## activate won't change it if not specified
  # tidync(ufile, 4) %>% activate()
  # tidync(ufile) %>% activate(handbook5Id)
  # tidync(ufile)  %>% activate("handbook5Id")
  # tidync(ufile)  %>% activate("D8,D7")

})
