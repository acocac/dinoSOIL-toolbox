setwd("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M")
library(raster)
library(rgdal)
library(raster)
library(caret)
library(doMC)
library(plyr)
library(doParallel) 
library(dismo)
library(readxl)
library(aqp)
library(raster)
library(aqp)
library(sp)

COV <- stack("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\COV_COLOR\\COV_VF.tif")
names(COV) <- readRDS("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\COV_COLOR\\NAMES_COV_VF_COLOR.rds")
names(COV)
COV
plot(COV[[30]])

#importacia de 1_covariables para orden
dat_subset <- read.csv('E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\COV_COLOR\\RegMatrix_VF_observaciones.csv',sep=";")
names(dat_subset)
head(dat_subset)
library(raster)
library(caret)
library(doMC)
library(doParallel)
names(dat_subset)
dim(dat_subset)
summary(dat_subset)
dat_subset <- na.omit(dat_subset)
dim(dat_subset)
dat_subset$orden <- factor(dat_subset$orden)
dat_subset$Cobertura <- factor(dat_subset$Cobertura)
dat_subset$dip_1<- factor(dat_subset$dip_1)
dat_subset$naf_1 <- factor(dat_subset$naf_1)
dat_subset$dip_2 <- factor(dat_subset$dip_2)
dat_subset$naf_2 <- factor(dat_subset$naf_2)
dat_subset$T_Relieve <- factor(dat_subset$T_Relieve)
dat_subset$F_TERRENO <- factor(dat_subset$F_TERRENO)
dat_subset$MAT_PARENTAL <- factor(dat_subset$MAT_PARENTAL)

str(dat_subset)
names(dat_subset)
table(dat_subset$suborden)


cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatdcv", number=10, repeats=10)
(RFE_RF_model <- rfe(dat_subset[-c(1:26,32,49,60,28)], dat_subset[,6], sizes=c(1:10), rfeControl=control2))
plot(RFE_RF_model, type=c("g", "o"))
predictors(RFE_RF_model)[1:5]
(RFE_RF_model2 <- rfe(dat_subset[predictors(RFE_RF_model)[1:5]], dat_subset[,33], sizes=c(1:6), rfeControl=control2) )
predictors(RFE_RF_model2)
stopCluster(cl = cl)


names(dat_subset)
names(COV)
plot(COV84[[2]])
fm = as.formula(paste("orden~", paste0(names(COV)[c(23,6,34,9,3,8,10,7,21)],collapse = "+"))) 
fm
library(randomForest)
library(caret)


##RANDOM FOREST
# Default 10-fold cross-validation
ctrl <- trainControl(method = "cv", savePred=T)

# Search for the best mtry parameter
names(dat_subset)
library(caret)
library(randomForest)
dat_subset_sp <- dat_subset
coordinates(dat_subset_sp) <- ~ Este + Norte
str(dat_subset_sp)
names(dat_subset_sp)
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy
#ls(getModelInfo())
(rfmodel <- train(fm, data=dat_subset_sp@data, method = "rf", trControl = ctrl, 
                  importance=TRUE))
rfmodel$pred[,1]
xtab <- table(rfmodel$pred[,1], rfmodel$pred[,2])
con.mat <- confusionMatrix(xtab)
con.mat


varImpPlot(rfmodel[11][[1]])
plot(rfmodel[11][[1]])
pred <- predict(COV, rfmodel,filename = "E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\COV_COLOR\\RF_5m_PRED_orden_18092018_v2.tif",
                format = "GTiff", overwrite = T)
plot(pred)








######========TRUCO PARA QUE SIRVA RFE=====
dat_subset <- read.csv("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\rmat.csv",sep=";")
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=nbFuncs, method="repeatedcv", number=5, repeats=5)
(RFE_RF_model <- rfe(dat_subset[-c(1:4,23:31)], factor(dat_subset[,28]), sizes=c(1:6), rfeControl=control2) )
plot(RFE_RF_model, type=c("g", "o"))
######========TRUCO PARA QUE SIRVA RFE
