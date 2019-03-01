
 context("activate")

 test_that("generic meaningless activation works", {
   l <- list(a = 1, b = 2)
   expect_warning(l <- activate(l, "b"))
   expect_warning(active(l) <- "b")
   f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
   l3file <- system.file("extdata/oceandata", f, package= "tidync")
   tnc <- tidync(l3file)
   expect_identical(tnc, activate(tnc))
 }
)
 test_that("various access and activation works", {

   ufile <- system.file("extdata", "unidata", "madis-hydro.nc", package = "tidync")
   tnc <- tidync(ufile)

  ## changed with nesting of variables in nc_grids https://github.com/hypertidy/ncmeta/issues/26
  #expect_equal(active(tidync(ufile)) , "D0,D12")
  expect_equal(active(tidync(ufile)) , "D5,D12")
  
  
  ## use variable to find grid
  tnc1 <-  tnc %>% activate(handbook5Id)
  expect_equal(active(tnc1),  "D1,D12")
  
  ## use number to find grid
  
  tnc2 <- tnc %>% activate(3)
  expect_equal(active(tnc2), "D10,D9")
  
  expect_error(activate(tnc2, 0))

  
  expect_error(activate(tnc2, snarfleglobber), "not found")
  snarfleglobber <-   "snarfleglobber "
  expect_error(activate(tnc2, snarfleglobber), "Only possible")
  
  expect_warning(activate(1, "a"))

})

 