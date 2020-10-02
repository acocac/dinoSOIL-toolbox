##=====================================================================================
#Codigo necesario para el mapeo de pH a los intervalos de profundidad deseados
#0-30 cm
#30-100 cm
#======================================================================================
rm(list=ls())

setwd("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\CODIGOS")
{
library(raster)
library(rgdal)
library(sp)
library(magrittr)
library(raster)
library(readxl)
library(tidyr)
library(hddtools)
library(caret)
library(doParallel)
library(randomForest)
library(SuperLearner)
library(Boruta)
}

##Carga de de matriz de regresion
data <- read.csv("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\BASES\\MatrixRegresion_29092020.csv",sep=";")


##Carga de covariables y nombres
cov <- stack("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\COVARIABLES\\Covariables_PT_2020.tif")
names(cov) <- readRDS("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\COVARIABLES\\NombresCovariables_PT_2020.rds")


#=============================
#Modelo Orden de suelos
#=============================
names(data)
data <- data[,-c(1:5,7:16)] %>% na.omit
data$ORDEN <- factor(data$ORDEN)

data <- downSample(as.matrix(data[,-1]), data[,1])
dim(data)
names(data)
summary(data)

### Conjunto de datos para entrenamiento y para validacion
set.seed(149)
inTrain <- createDataPartition(y = data$Class, p = .70, list = FALSE) 
train_data <- data[ inTrain,]
dim(train_data)
test_data <- data[-inTrain,]
dim(test_data)

names(data)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10, repeats=10)
(rfmodel <- rfe(x=data[,-27], y=data[,27], sizes=c(1:10), rfeControl=control2))
plot(rfmodel, type=c("g", "o"))
predictors(rfmodel)[1:10]

##Seleccion de variables --> algoritmo Boruta
start <- Sys.time()
(bor <- Boruta(Class ~ ., data = data, doTrace = 0, ntree = 500,maxRuns=500))
plot(bor, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(bor$ImpHistory),function(i)
  bor$ImpHistory[is.finite(bor$ImpHistory[,i]),i])
names(lz) <- colnames(bor$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(bor$ImpHistory), cex.axis = 0.7)
print(Sys.time() - start)

print(bor)
names(bor$finalDecision[bor$finalDecision %in% c("Confirmed")])

#MODELO1

ctrl <- caret::trainControl(method = "cv",
                            number = 10,
                            verboseIter = FALSE,
                            summaryFunction = multiClassSummary,
                            sampling = "down")
fm <-  as.formula(paste0("Class~",paste0(as.character(predictors(rfmodel)[c(1:10)]),collapse = "+")))
fm
ls(getModelInfo())
set.seed(472)
model_rf_under <- caret::train(fm, 
                               data = train_data,
                               method = "ranger",
                               trControl = ctrl)

#data for validation
val <- predict(model_rf_under,test_data)
val <- factor(val)
data.frame(levels(val))
caret::confusionMatrix(val,test_data[,27])

pred <- predict(cov[[predictors(rfmodel)[c(1:10)]]],
                model_rf_under,
                filename = "E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\SALIDAS\\RANGER_ORDEN_30092020.tif",
                format = "GTiff", overwrite = T)
plot(pred)

##MODELO 2

ctrl <- caret::trainControl(method = "cv",
                            number = 10,
                            verboseIter = FALSE,
                            summaryFunction = multiClassSummary,
                            sampling = "up")
fm <-  as.formula(paste0("Class~",paste0(as.character(predictors(rfmodel)[c(2:8)]),collapse = "+")))
fm
ls(getModelInfo())
set.seed(472)
model_rf_under <- caret::train(fm, 
                               data = train_data,
                               method = "nnet",
                               trControl = ctrl)

#data for validation
val <- predict(model_rf_under,test_data)
val <- factor(val)
caret::confusionMatrix(val,test_data[,23])

pred <- predict(cov[[predictors(rfmodel)[c(2:8)]]],
                model_rf_under,filename = "NNET_SUBORDEN_15092020.tif",
                format = "GTiff", overwrite = T)
plot(pred)


####MODELO3
ctrl <- caret::trainControl(method = "cv",
                            number = 10,
                            verboseIter = FALSE,
                            summaryFunction = multiClassSummary,
                            sampling = "up")
fm <-  as.formula(paste0("Class~",paste0(as.character(predictors(rfmodel)[c(2:8)]),collapse = "+")))
fm
ls(getModelInfo())
set.seed(472)
model_rf_under <- caret::train(fm, 
                               data = train_data,
                               method = "svmLinear",
                               trControl = ctrl)

#data for validation
val <- predict(model_rf_under,test_data)
val <- factor(val)
caret::confusionMatrix(val,test_data[,23])

pred <- predict(cov[[predictors(rfmodel)[c(2:8)]]],
                model_rf_under,filename = "SVML_SUBORDEN_15092020.tif",
                format = "GTiff", overwrite = T)
plot(pred)


####MODELO4
ctrl <- caret::trainControl(method = "cv",
                            number = 10,
                            verboseIter = FALSE,
                            summaryFunction = multiClassSummary,
                            sampling = "up")
fm <-  as.formula(paste0("Class~",paste0(as.character(predictors(rfmodel)[c(2:8)]),collapse = "+")))
fm
ls(getModelInfo())
set.seed(472)
model_rf_under <- caret::train(fm, 
                               data = train_data,
                               method = "regLogistic",
                               trControl = ctrl)

#data for validation
val <- predict(model_rf_under,test_data)
val <- factor(val)
caret::confusionMatrix(val,test_data[,23])

pred <- predict(cov[[predictors(rfmodel)[c(2:8)]]],
                model_rf_under,filename = "RLML_SUBORDEN_15092020.tif",
                format = "GTiff", overwrite = T)
plot(pred)

names(cov[[predictors(rfmodel)[c(1:7)]]])
#https://www.analyticsvidhya.com/blog/2017/02/introduction-to-ensembling-along-with-implementation-in-r/
#https://www.analyticsvidhya.com/blog/2015/08/introduction-ensemble-learning/


