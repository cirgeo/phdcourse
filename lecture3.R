library(terra) 
## "require" is like "library" but returns true/false,
## false is returned if package does not exist
if(!require(rminer)) install.packages("rminer")

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
                                  method="random" )
print(names(sampled.Grid))
 

## use rminer package ( ?rminer::mining for info)  -----
models=c("naive","ctree","rpart","kknn",
         "mlp","mlpe","ksvm",
         "randomForest","mr","mars",
         "plsr")
## prepare an empty list for storing accuracy metrics -----
rmse <- c()
rsquared <- c()
bestmodel <- NULL
for(model in models)
{ 
  M=rminer::mining(ndviX1000~.,data=sampled.Grid,method=c("holdout",2/3,12345),model=model)
  ##get all accuracy metrics 
  met <-mmetric(M, metric="ALL")
  if( is.null(bestmodel) || met$RMSE < min( rmse ) ){
    bestmodel<- model
  }
  
  rmse  <- c(rmse, met$RMSE)
  rsquared <- c(rsquared, met$R2)
  cat("model:",model,"RMSE:",round(met$RMSE,digits=3),"\n")
}
names(rmse ) <- models
names(rsquared ) <- models

cat("Best model is: ", bestmodel, "... fitting...\n")

## apply the best machine learning algo for regression (reg) -----
FIT <- rminer::fit(ndviX1000~.,data=sampled.Grid, 
            model=bestmodel, task = "reg" )

# create a 10x smaller image to predict faster  -----
myVarRescale.small <- terra::aggregate(myVarRescale, 10)

## predict the final NDVI raster using the model! -----
predicted.NDVI <- terra::predict( myVarRescale.small,  
                                  FIT, 
                                  na.rm=T)
 
  
terra::writeRaster(predicted.NDVI, "myPredictedNDVIraster.tif", overwrite=T)

## calculate residuals using the original NDVI -----
residualsNDVI <- predicted.NDVI - myVarRescale.small$ndviX1000

## plot predicted NDVI and "errors", aka residuals   -----
plot(predicted.NDVI)
plot(residualsNDVI)
## plot residuals in boxplot -----
boxplot(residualsNDVI, outline=FALSE)
## plot residuals distribution in histogram  -----
hist(residualsNDVI, breaks=100, main="Residuals" )
## calculate RMSE -----
rmse <- sqrt(mean(residualsNDVI[]^2, na.rm=TRUE)) 
## plot -----
smoothScatter(y= predicted.NDVI[], x=myVarRescale.small$ndviX1000[],
              ylab="Predicted NDVI", xlab="Measured NDVI",
              asp=1, main= paste("RMSE: ", round(rmse,3)))

linear.fit <- lm(predicted.NDVI[] ~  myVarRescale.small$ndviX1000[])
summary(linear.fit)
abline(linear.fit, lwd=2, col="red")

