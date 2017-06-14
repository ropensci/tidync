f <- "/rdsi/PRIVATE/raad/data/oceandata.sci.gsfc.nasa.gov/SeaWiFS/Mapped/Monthly/9km/chlor/S20092742009304.L3m_MO_CHL_chlor_a_9km.nc"
file.copy(f, file.path("inst/extdata/oceandata", basename(f)))
