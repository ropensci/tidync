context("axis")


test_that("axes works on various files", {
  skip_if_not(we_are_raady())
  oisst_dayfile <- raadtools::sstfiles()$fullname[1]
  oisst_monfile <- raadtools::sstfiles(time.resolution = "monthly")$fullname[1]
  roms_file <- raadtools::cpolarfiles()$fullname[1]
  
  lat_axis <- tidync(oisst_dayfile) %>% axis_transforms() %>% `[[`("lat") 
  expect_true(all(lat_axis$selected))
  expect_that(nrow(lat_axis), equals(720L))
  
  
  tidync(oisst_monfile ) %>% axis_transforms()
  tidync(oisst_dayfile) %>% axis_transforms()
  
})
