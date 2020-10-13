##=====================================================================================
#Codigo necesario para el mapeo de pH a los intervalos de profundidad deseados
#0-30 cm
#30-100 cm
#======================================================================================
rm(list=ls())

source('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/src/functions/5_Predict.R')

proyecto.directorio <- '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena'
datos.salida <- paste0(proyecto.directorio,'/datos/salida/1_predicciones')
dir.create(datos.salida, recursive = T, mode = "0777", showWarnings = F)

# Librerias
pckg <- c('raster', 'rgdal', 'sp', 'magrittr', 'readxl', 'tidyr',
          'hddtools', 'caret', 'doParallel', 'randomForest', 'SuperLearner', 'Boruta')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

lapply(pckg,usePackage)

##Carga de de matriz de regresion
data <- read.csv("/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/tabular/MatrixRegresion_29092020.csv",sep=";")


##Carga de 1_covariables y nombres
cov <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/covariables/Covariables_PT_2020.tif')
names(cov) <- readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/rds/NombresCovariables_PT_2020.rds')


#=============================
#Modelo Orden de suelos
#=============================
names(data)
data <- data[,-c(1:5,7:16)] %>% na.omit
data$ORDEN <- factor(data$ORDEN)
table(data$ORDEN)

data <- downSample(as.matrix(data[,-1]), data[,1])
dim(data)
names(data)
summary(data)
table(data$Class)

### Conjunto de datos para entrenamiento y para validacion
set.seed(149)
inTrain <- createDataPartition(y = data$Class, p = .70, list = FALSE) 
train_data <- data[ inTrain,]
dim(train_data)
test_data <- data[-inTrain,]
dim(test_data)

names(data)
cl <- makeCluster(detectCores()-1, type='PSOCK')
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
fm <-  as.formula(paste0("Class~",paste0(as.character(predictors(rfmodel)[c(1:7)]),collapse = "+")))
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

modelos.mejor <- 'ranger'
PredictGeoTIFF(cov[[predictors(rfmodel)[c(1:7)]]], model_rf_under, datos.salida, modelos.mejor)

pred <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_predicciones/ranger_5m_PRED_orden_18092018_v2.tif')

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


