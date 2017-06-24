context("hyper_index")

f <- system.file("extdata", "S2008001.L3m_DAY_CHL_chlor_a_9km.nc", package = "ncmeta")
tidync(f)
## also make sure we can active a new grid, this is the dimension ref comma separated (no brackets)
nc <- tidync(f) %>% activate("D1,D0")
fnc <- nc %>% hyper_filter()
test_that("building the index  works", {
  fnc %>% hyper_index()
  nc %>% hyper_filter(lon = lon > 170, lat = index > 2000) %>% hyper_index()
})
