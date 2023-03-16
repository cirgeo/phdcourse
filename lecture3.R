library(terra)
library(autocart)

myVariables <- terra::rast("data/variables.tif") 

ndvi <- (myVariables$ndviX1000/1000)-1
meanTemp <- myVariables$MeanTempX100/100
stddevTemp <- myVariables$StdDevTempX100/100

myVarRescale <- c( myVariables$Elevation+0 ,
                             ndvi, meanTemp, stddevTemp)

#### sampling -----
nsamples <- 2000
sampled.Grid <- terra::spatSample(myVarRescale, 
                                  size=nsamples, 
                                  method="random", 
                                  cells=T)

sampled.Grid$LONGITUDE <- terra::xFromCell(myVarRescale, sampled.Grid$cell)
sampled.Grid$LATITUDE  <- terra::yFromCell(myVarRescale, sampled.Grid$cell)

plot(sampled.Grid$LONGITUDE, sampled.Grid$LATITUDE)

response <- as.matrix(sampled.Grid$ndviX1000)

predictors <- sampled.Grid
predictors$ndviX1000 <- NULL

locations <- as.matrix(cbind(sampled.Grid$LONGITUDE, sampled.Grid$LATITUDE))

numtraining <- round(0.5 * nsamples)
training_index <- rep(FALSE, nsamples)
training_index[1:numtraining] <- TRUE
training_index <- sample(training_index)



train_response <- response[training_index]
test_response <- response[!training_index]
train_predictors <- predictors[training_index, ]
test_predictors <- predictors[!training_index, ]
train_locations <- locations[training_index, ]
test_locations <- locations[!training_index, ]

