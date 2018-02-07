ufun <- "https://oceandata.sci.gsfc.nasa.gov/cgi/getfile"
f <- "S20080012008031.L3m_MO_CHL_chlor_a_9km.nc"
curl::curl_download(file.path(ufun, f), 
                    file.path("inst", "extdata", "oceandata", f), mode = "wb")


#oc <- raadtools::ocfiles(product = "SeaWiFS")
#which.min(file.info(oc$fullname)$size)
f2 <- "S2008001.L3b_DAY_RRS.nc"
curl::curl_download(file.path(ufun, f2), file.path("inst", "extdata", "oceandata", f2), mode = "wb")
