context("files")

fname <- paste(sample(unlist(strsplit("somecrazyfile", ""))), collapse = "")
test_that("file not found is friendly", {
  expect_error(tidync(fname), "No such file or directory")
})

test_that("files and bad files are handled", {
  skip_if_not(we_are_raady())
  oisst_dayfile <- raadtools::sstfiles()$fullname[1]
  tidync(oisst_dayfile)
  oisst_monfile <- raadtools::sstfiles(time.resolution = "monthly")$fullname[1]
  tidync(oisst_monfile)
  roms_file <- raadtools::cpolarfiles()$fullname[1]
  tidync(roms_file)
  
  l3_file <- raadtools::ocfiles()$fullname[1]  
   tidync(l3_file)
})


## FIXME: we need a lot more examples!  
test_that("non-local sources work", {
  skip_on_travis()
  u <- "http://coastwatch.pfeg.noaa.gov/erddap/griddap/erdQSwind3day"
  (unc <- tidync(u))
  
  unc %>% activate("D2")
  
  uhf <- unc %>% hyper_filter(longitude = abs(index - 100) < 10, latitude = latitude > 60, time = index == 1)
  
  res_uv_wind <- hyper_tibble(uhf) 
})
