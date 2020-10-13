##=====================================================================================
#Codigo necesario para el mapeo de pH a los intervalos de profundidad deseados
#0-30 cm
#30-100 cm
#======================================================================================
rm(list=ls())

source('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/src/functions/5_Predict.R')

proyecto.directorio <- '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena'
exploratorio.variables.figuras = paste0(proyecto.directorio,'/exploratorio/figuras')
dir.create(exploratorio.variables.figuras, recursive = T, mode = "0777", showWarnings = F)
exploratorio.variables.rds = paste0(proyecto.directorio,'/exploratorio/rds')
dir.create(exploratorio.variables.rds, recursive = T, mode = "0777", showWarnings = F)
datos.salida <- paste0(proyecto.directorio,'/datos/salida/1_predicciones')
dir.create(datos.salida, recursive = T, mode = "0777", showWarnings = F)

# Librerias
pckg <- c('raster', 'rgdal', 'sp', 'magrittr', 'readxl', 'tidyr','soilassessment', 'hydroGOF',
          'hddtools', 'caret', 'doParallel', 'snow', 'randomForest', 'SuperLearner', 'Boruta')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

lapply(pckg,usePackage)

#-----------------------------
#Carga de matriz de regresion
#-----------------------------
data <- read.csv("/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/tabular/MatrixRegresion_29092020.csv",sep=";")


#-------------------------------
#Carga de variables ambientales
#-------------------------------
cov <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/covariables/Covariables_PT_2020.tif')
names(cov) <- readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/rds/NombresCovariables_PT_2020.rds')


#----------------------
#Modelo pH 0-30 cm
#----------------------
names(data)
data <- data[,-c(1:12,14:16,24,40,41)] %>% na.omit#pH0-30 y variables ambientales


##Conjunto de datos para entrenamiento y para validacion
set.seed(225)
inTrain <- createDataPartition(y = data[,1], p = .70, list = FALSE)
train_data <- data[ inTrain,]
dim(train_data)
test_data <- data[-inTrain,]
dim(test_data)

##Seleccion de variables --> RFE
names(data)
start <- Sys.time()
cl <- makeCluster(detectCores()-1, type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=10, repeats=10)
(rfmodel <- rfe(x=data[,-1], y=data[,1], sizes=c(1:10), rfeControl=control2))
  #boxplots charts
png(file = paste0(exploratorio.variables.figuras,'/pH0-30-spline_rfe.png'), width = 700, height = 600)
plot(rfmodel, type=c("g", "o"))
dev.off()
predictors(rfmodel)[1:3]
save(rfmodel, file=paste0(exploratorio.variables.rds,'/pH0-30-spline_rfe.rds'))
print(Sys.time() - start)
names()
##Seleccion de variables --> algoritmo Boruta
start <- Sys.time()
(bor <- Boruta(pH.0_30_spline ~ ., data = data, doTrace = 0, ntree = 500,maxRuns=500))
png(file = paste0(exploratorio.variables.figuras,'/pH0-30-spline_boruta.png'), width = 700, height = 600)
plot(bor, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(bor$ImpHistory),function(i)
  bor$ImpHistory[is.finite(bor$ImpHistory[,i]),i])
names(lz) <- colnames(bor$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(bor$ImpHistory), cex.axis = 0.7)
dev.off()
print(Sys.time() - start)

print(bor)
names(bor$finalDecision[bor$finalDecision %in% c("Confirmed")])

# final.bor <- TentativeRoughFix(bor)
# print(final.bor)
# 
# getSelectedAttributes(bor, withTentative = F)
# boruta.df <- attStats(final.bor)


##Evaluacion de varios modelos
data1 <- cbind(pH0.30=data$pH.0_30_spline, data[,predictors(rfmodel)[c(1:3)]])
names(data1)
start <- Sys.time()
regmodelSuit(data1,pH0.30,VD,CHBL,DEM)
print(Sys.time() - start)

##Entrenar modelo seleccionado
ls(getModelInfo())##Modelos disponibles en el paquete caret (solo informativo)

fm <-  as.formula(paste0("pH.0_30_spline~",paste0(as.character(predictors(rfmodel)[c(1:3)]),collapse = "+")))
fm

ctrl <- trainControl( method = "cv",
                      number=10,
                      returnResamp = "all",
                      savePredictions = TRUE, 
                      search = "random",
                      verboseIter = FALSE
)

# Calculate the number of cores
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
doParallel::registerDoParallel(cl)                #register the cluster

## foreach or lapply would do this faster
set.seed(40)
modelo <- train(fm,  
                data = train_data,  
                method = "ranger",
                trControl=ctrl
)
stopCluster(cl = cl)

pred = predict(modelo, test_data[,predictors(rfmodel)[c(1:3)]], onlySL = TRUE)
cor(pred,test_data$pH.0_30_spline)

(AVE <- 1 - sum((pred-test_data$pH.0_30_spline)^2, na.rm=TRUE)/
    sum((test_data$pH.0_30_spline - mean(test_data$pH.0_30_spline, na.rm = TRUE))^2,
        na.rm = TRUE)
)

hydroGOF::rmse(pred,test_data$pH.0_30_spline)

modelos.mejor <- 'ranger'
outvar = 'pH0-30-spline'
PredictGeoTIFF(cov[[predictors(rfmodel)[c(1:3)]]], modelo, datos.salida, modelos.mejor, outvar)

pred <- predict(cov[[predictors(rfmodel)[c(1:3)]]],modelo,
                filename = "E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\SALIDAS\\RANGER_pH030SumPond_30092020.tif",
                format = "GTiff", overwrite = T)
plot(pred)






##Modelo ensamblado por defecto
listWrappers()
names(train_data)
ens.model <- SuperLearner(Y=train_data[,1],
                          X=train_data[,predictors(rfmodel)[c(1:3)]],
                          family=gaussian(),
                          SL.library=list("SL.ranger","SL.ksvm"))

ens.model

pred = predict(ens.model, test_data[,predictors(rfmodel)[c(1:3)]], onlySL = TRUE)
cor(pred$pred,test_data$pH.0_30_Sum.Pond)

(AVE <- 1 - sum((pred$pred-test_data$pH.0_30_Sum.Pond)^2, na.rm=TRUE)/
    sum((test_data$pH.0_30_Sum.Pond - mean(test_data$pH.0_30_Sum.Pond, na.rm = TRUE))^2,
        na.rm = TRUE)
)

hydroGOF::rmse(pred$pred,data.frame(test_data$pH.0_30_Sum.Pond))



SL.ranger.mod <- function(...){
  SL.ranger(..., num.trees=500, mtry=2,quantreg=F)
}

SL.ksvm.mod <- function(...){
  SL.ksvm(..., C=8.427125, kernel="polydot", scaled=T)
}

##Modelo ensamblado "tuned"
ens.model.mod <- SuperLearner(Y=train_data[,1],
                              X=train_data[,predictors(rfmodel)[c(1:3)]],
                              family=gaussian(),
                              SL.library=list("SL.ranger.mod","SL.ksvm.mod"))
ens.model.mod

pred = predict(ens.model.mod, test_data[,predictors(rfmodel)[c(1:3)]], onlySL = TRUE)
cor(pred$pred,test_data$pH.0_30_Sum.Pond)

(AVE <- 1 - sum((pred$pred-test_data$pH.0_30_Sum.Pond)^2, na.rm=TRUE)/
    sum((test_data$pH.0_30_Sum.Pond - mean(test_data$pH.0_30_Sum.Pond, na.rm = TRUE))^2,
        na.rm = TRUE)
)

hydroGOF::rmse(pred$pred,data.frame(test_data$pH.0_30_Sum.Pond))


#https://www.analyticsvidhya.com/blog/2017/02/introduction-to-ensembling-along-with-implementation-in-r/
#https://www.analyticsvidhya.com/blog/2015/08/introduction-ensemble-learning/


