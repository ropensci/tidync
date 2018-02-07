context("axis")


test_that("axes works on various files", {
  
  #lat_axis <- tidync(oisst_dayfile) %>% axis_transforms() %>% `[[`("lat") 
  #expect_true(all(lat_axis$selected))
  #expect_that(nrow(lat_axis), equals(720L))
  
  
  #tidync(oisst_monfile ) %>% axis_transforms() %>% 
  #  expect_length(4L) %>% expect_named(c("lon", "lat", "time", "nbnds"))
  #tidync(oisst_dayfile) %>% axis_transforms() %>% 
  #  expect_length(4L) %>% expect_named(c("lon", "lat", "zlev", "time"))
  
  #roms_names <- c("xi_rho", "eta_rho", "s_rho", "ocean_time", "s_w", "xi_u", 
  #                "eta_u", "xi_v", "eta_v", "tracer", "boundary", "xi_psi", "eta_psi")
  #axes <- tidync(roms_file)$transforms 
  #axes %>% expect_length(13L) %>% 
  #  expect_named(roms_names)
  #axes_summary <- structure(c(1443L, 392L, 31L, 31L, 32L, 1442L, 392L, 1443L, 391L, 
  #            2L, 4L, 1442L, 391L), .Names = roms_names)
  
#  unlist(lapply(axes, nrow)) %>% expect_equal(axes_summary)
})
