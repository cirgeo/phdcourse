
# IMPORT LIBRARIES ####
library("terra")
?terra::rast

mymessage <- "hello"
mynumber <- 12

class(mynumber)
class(mymessage)

#### READ DTM RASTER ####
r <- terra::rast("mySRTM.tif")
terra::plot(r)
