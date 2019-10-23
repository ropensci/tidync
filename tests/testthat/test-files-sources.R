context("files")

fname <- paste(sample(unlist(strsplit("somecrazyfile", ""))), collapse = "")
test_that("file not found is friendly", {
  skip_on_cran()
  expect_error(tidync(fname))
})

test_that("files and bad files are handled", {
  skip_on_cran()
  l3b_file <- system.file("extdata/oceandata/S2008001.L3b_DAY_RRS.nc", 
                          package = "tidync", mustWork = TRUE)
  expect_error(suppressWarnings(tidync(l3b_file)), 
               "no variables or dimensions") 
  expect_warning(try(tidync(l3b_file), silent = TRUE), 
                 "no dimensions found")
  expect_warning(try(tidync(l3b_file), silent = TRUE), 
                 "no variables found")
  expect_warning(try(tidync(l3b_file), silent = TRUE), 
                 "no variables recognizable")  
  f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"  
  l3file <- system.file("extdata/oceandata", f, 
                        package= "tidync", mustWork = TRUE)
  
  expect_warning(try(tidync(l3file[c(1, 1)])), 
                 "only one source allowed, first supplied chosen")
  tfile <- tempfile()
  nothingfile <- RNetCDF::create.nc(tfile)
  RNetCDF::close.nc(nothingfile)
  
  expect_warning(ouch <- try(tidync::tidync(tfile), silent = TRUE), 
                 "no dimensions found")
  
  
  tnc <- tidync(l3file)
  tnc$grid <- tnc$grid[0, ]
  expect_output(print(tnc))
})

test_that("verbs have a character method", {
  skip_on_cran()
  f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
  l3file <- system.file("extdata/oceandata", f, 
                        package= "tidync", mustWork = TRUE)
  hyper_tibble(l3file, lon = lon < -140, lat = lat > 85) %>% 
    expect_s3_class("tbl_df")
  hyper_filter(l3file, lon = lon < -140, lat = lat > 85) %>% 
    expect_s3_class("tidync")
  expect_warning(hyper_slice(l3file, 
                             lon = lon < -140, 
                             lat = lat > 85, 
                             select_var = "chlor_a")) %>% 
    expect_type("list")
  expect_silent(hyper_array(l3file, lon = lon < -140, lat = lat > 85, 
                            select_var = "chlor_a")) %>% 
    expect_type("list")
  
  hyper_tbl_cube(l3file, lon = lon < -140, lat = lat > 85) %>% 
    expect_s3_class("tbl_cube")
  
})
test_that("RNetCDF fall back works", {
  skip_on_cran()
  ufile <- system.file("extdata", "unidata", "madis-hydro.nc", 
                       package = "tidync", mustWork = TRUE)
  nc_get(ufile, "invTime") %>% expect_is("array") %>% 
    expect_length(1176) #%>% expect_type("double")
  nc_get(ufile, "invTime", test = TRUE) %>% expect_is("array") %>% 
    expect_length(1176) #%>% expect_type("integer")
  
  expect_output({
  expect_error(nc_get(ufile, "notavariable"))
  expect_error(nc_get(ufile, "notavariable", test = TRUE))
  })
})

