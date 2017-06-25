context("files")

fname <- paste(sample(unlist(strsplit("somecrazyfile", ""))), collapse = "")
test_that("file not found is friendly", {
  expect_error(tidync(fname), "No such file or directory")
})

test_that("files and bad files are handled", {
  #skip_if_not(we_are_raady())
  oisst_dayfile <- raadtools::sstfiles()$fullname[1]
  tidync(oisst_dayfile)
  oisst_monfile <- raadtools::sstfiles(time.resolution = "monthly")$fullname[1]
  tidync(oisst_monfile)
  roms_file <- raadtools::cpolarfiles()$fullname[1]
  tidync(roms_file)
  
  l3_file <- raadtools::ocfiles()$fullname[1]  
   tidync(l3_file)
   })

