library(terra)

myVariables <- terra::rast("data/variables.tif")
plot(  myVariables$Elevation  )
names(myVariables)

originalNames <- names(myVariables)
names(myVariables) <- c("A", "B", "C", "D")
names(myVariables)
names(myVariables) <- originalNames

ndvi <- (myVariables$ndviX1000/1000)-1
meanTemp <- myVariables$MeanTempX100/100
stddevTemp <- myVariables$StdDevTempX100/100

myVarRescale <- c( myVariables$Elevation+0 ,
                             ndvi, meanTemp, stddevTemp)

# terra::crs(myVarRescale)
# myFirstRaster.3035 <- terra::project(myVariables$Elevation, "epsg:3035")
# plot(myFirstRaster.3035)
# png("varPlot.png", width = 2600, height=1800, res=400)
#  plot(myVariables)
# dev.off()

# pdf("ndviHist.pdf"  )
#  terra::hist(myVarRescale$ndviX1000)
#  terra::hist(myVarRescale$MeanTempX100)
# dev.off()

vector.raster.ndvi <- terra::values(myVarRescale$ndviX1000)
vector.raster.elevation <- myVarRescale$Elevation[]



vector.raster.matrix <- myVarRescale[]
vector.raster.table <- as.data.frame(myVarRescale[])


correlation.table <- cor(vector.raster.table)

lm.fit <- lm(vector.raster.table$MeanTempX100 ~ vector.raster.table$Elevation)
summary(lm.fit)

png("correlationPlot.png", width = 2000, height=2000, res=300)
  smoothScatter( vector.raster.table$Elevation, vector.raster.table$MeanTempX100)
  abline(lm.fit, lwd=2, col="red")
dev.off()

smoothScatter( vector.raster.table$Elevation, vector.raster.table$MeanTempX100)
abline(lm.fit, lwd=2, col="red")


#### sampling -----

sampled.Grid <- terra::spatSample(myVarRescale, size=2000, method="regular", cells=T)

sampled.Grid$LONGITUDE <- terra::xFromCell(myVarRescale, sampled.Grid$cell)
sampled.Grid$LATITUDE  <- terra::yFromCell(myVarRescale, sampled.Grid$cell)

response <- as.matrix(sampled.Grid$ndviX1000)
predictors <- sampled.Grid
predictors$ndviX1000 <- NULL
locations <- as.matrix(cbind(snow$LONGITUDE, snow$LATITUDE))

