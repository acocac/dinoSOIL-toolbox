library(raster)
library(rgdal)
setwd("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M")
dir()
dem <- raster("E:\\SIBUNDOY\\SIBUNDOY_covariables\\DERIVADOS_DEM_05\\DEM_05.tif")
dem

#####COV SIBUNDOY_5M

files <- list.files(getwd(), pattern="tif$", full.names=F)
files 
for(i in files) { 
  assign(unlist(strsplit(i, "[.]"))[1], 
         raster(i)) 
} 
ls()

COV <- stack(files)
names(COV84)
writeRaster(COV,"COV_SIB.tif",format="GTiff",overwrite=T)
saveRDS(names(COV),"NAMES_COV_SIB.rds")

crop
############Preparacion de Covariables##############
dir()


ppt <- raster("E:\\SIBUNDOY\\SIBUNDOY_covariables\\PRECIPITACION\\PPT_CLIP.tif")
ppt <- projectRaster(ppt, COV)
COV2 <- stack(ppt,COV)
names(COV2)


setwd("E:\\SIBUNDOY\\SIBUNDOY_covariables\\DERIVADOS_DEM_05")



files <- list.files(getwd, pattern="tif$", full.names=F)
files 
for(i in files) { 
  assign(unlist(strsplit(i, "[.]"))[1], 
         raster(i)) 
} 
COV_dem <- stack(files)
names(COV_dem)
COV3 <- stack(COV2,COV_dem)
names(COV3)




ndvi <- raster("E:\\SIBUNDOY\\SIBUNDOY_covariables\\NDVI\\NDVI_SIBUNDOY_CLIP.tif")
ndvi <- projectRaster(ndvi, COV3)
COV4 <- stack(COV3,ndvi)
names(COV4)
getwd()
setwd("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M")
writeRaster(COV4,filename = "COV_SIBU.tif",format="GTiff",overwrite=T)
saveRDS(names(COV4),"NAMES_COV_SIB.rds")


#####Covariable cobertura#############
start <- Sys.time()
COB <- readOGR(dsn="E:\\SIBUNDOY\\SIBUNDOY_covariables\\COBERTURAS\\VECTOR\\COBERTURAS_SIBUNDOY.shp")
COB_geog <- spTransform (COB, CRS=projection(COV84))
COB_geog_rasterized <- rasterize(COB_geog, COV84, 'CATEG')
COB_geog_rasterized <- resample(COB_geog_rasterized,COV84,method="bilinear")
names(COB_geog_rasterized) <- 'Cobertura'
COV4 <- stack(COV84, COB_geog_rasterized)
names(COV4)
writeRaster(COB_geog_rasterized, "E:\\SIBUNDOY\\SIBUNDOY_covariables\\COBERTURAS\\VECTOR\\COBERTURAS_SIBUNDOY.tif", format = "GTiff",overwrite=TRUE)
print(Sys.time() - start)


gl(21,2)
library(plyr)
#####Covariable suelos_paisaje#############
start <- Sys.time()
ORDEN <- readOGR(dsn="E:\\SIBUNDOY\\SIBUNDOY_covariables\\SUELOS\\VECTOR\\SUELOS_SIBUNDOY_VF_WGS84.shp")
names(ORDEN)
tabla_cats <- data.frame(read_excel("E:\\SIBUNDOY\\SIBUNDOY_covariables\\SUELOS\\VECTOR\\suelos_cat_VF.xls"))
names(tabla_cats)
ORDEN@data <- join(x=ORDEN@data,y=tabla_cats,by="OBJECTID")
names(ORDEN)
ORDEN_proj <- spTransform (ORDEN, CRS=projection(COV84))
proj4string(ORDEN_proj)

#####Covariable suelos_paisaje#############
ORDEN_proj_rasterized <- rasterize(ORDEN_proj, COV84, 'PAISAJE_CAT')
ORDEN_proj_rasterized <- resample(ORDEN_proj_rasterized,COV4,method="bilinear")
names(ORDEN_proj_rasterized) <- 'Paisaje'
COV4 <- stack(COV4, ORDEN_proj_rasterized)
names(COV4)

#####Covariable suelos_tipo de relieve#############
names(ORDEN_proj)
ORDEN_proj_rasterized <- rasterize(ORDEN_proj, COV84, 'T_REL_CAT')
ORDEN_proj_rasterized <- resample(ORDEN_proj_rasterized,COV4,method="bilinear")
names(ORDEN_proj_rasterized) <- 'Tipo_relieve'
COV4 <- stack(COV4, ORDEN_proj_rasterized)
names(COV4)

#####Covariable suelos_forma del terreno#############
names(ORDEN_proj)
ORDEN_proj_rasterized <- rasterize(ORDEN_proj, COV84, 'F_TERR_CAT')
ORDEN_proj_rasterized <- resample(ORDEN_proj_rasterized,COV84,method="bilinear")
names(ORDEN_proj_rasterized) <- 'Forma_terreno'
writeRaster(ORDEN_proj_rasterized, "E:\\SIBUNDOY\\SIBUNDOY_covariables\\SUELOS\\VECTOR\\FORMA_TERR_SIBUNDOY.tif", format = "GTiff",overwrite=TRUE)
COV4 <- stack(COV84, ORDEN_proj_rasterized)
names(COV4)

#####Covariable suelos_material parental#############
names(ORDEN_proj)
ORDEN_proj_rasterized <- rasterize(ORDEN_proj, COV4, 'MP_CAT')
ORDEN_proj_rasterized <- resample(ORDEN_proj_rasterized,COV4,method="bilinear")
names(ORDEN_proj_rasterized) <- 'Mat_parental'
COV4 <- stack(COV4, ORDEN_proj_rasterized)
names(COV4)

#####Covariable suelos_orden1#############
names(ORDEN_proj)
ORDEN_proj$OEDEN_1_CAT
ORDEN_proj_rasterized <- rasterize(ORDEN_proj, COV84, 'OEDEN_1_CAT')
ORDEN_proj_rasterized <- resample(ORDEN_proj_rasterized,COV4,method="bilinear")
names(ORDEN_proj_rasterized) <- 'Orden'
COV4 <- stack(COV4, ORDEN_proj_rasterized)
names(COV4)


writeRaster(ORDEN_proj_rasterized,filename = "ORDEN.tif",format="GTiff",overwrite=T)
writeRaster(COV4,filename = "COV_SIBU.tif",format="GTiff",overwrite=T)
saveRDS(names(COV4),"NAMES_COV_SIB.rds")
plot(COV4[[25]])
names(COV4)



#####MATRIZ DE REGRESION
library(readxl)
dat_subset <- read_excel("SIBUNDOY_VF.xlsx",sheet="ALL_DATA")
dim(dat_subset)
summary(dat_subset)
dat_subset <- dat_subset[complete.cases(dat_subset[,1:4]),]
names(dat_subset)
dat_subset$COS30CM <- as.numeric(dat_subset$COS30CM)
dat_subset$N_1 <- as.numeric(dat_subset$N_1)
dat_subset$W_1 <- as.numeric(dat_subset$W_1)
dat_subset$PERFIL <- as.factor(dat_subset$PERFIL)

str(dat_subset)

#### Juntamos covariables con los datos  ####
# Convert to spatial points df and project
library(sp)
dat_subset_sp <- dat_subset
coordinates(dat_subset_sp) <- ~ W_1 + N_1
str(dat_subset_sp)
dat_subset_sp
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy

setwd("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M")
library(raster)
dir()
COV84 <- stack("COV_WGS84_VF.tif")
names(COV84) <- readRDS("NAMES_COV_SIB_WGS84_VF.rds")
names(COV84)


start <- Sys.time()
#dat <- extract(COV84, dat_subset_sp, sp = TRUE)
dat_subset <- cbind(dat_subset, extract(COV84, dat_subset_sp))
summary(dat_subset)
dat_subset$PERFIL <- factor(dat_subset$PERFIL)
print(Sys.time() - start)

write.csv(dat_subset, 'RegMatrix_VF_Antes.csv')


write.csv(dat_subset, 'RegMatrix_VF.csv')

dat_subset_sp <- dat_subset
coordinates(dat_subset_sp) <- ~ W_1 + N_1
str(dat_subset_sp)
names(dat_subset_sp)
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy

#======RANDOM FOREST=====
########IMPORTANCIA DE LAS COVARIABLES
library(raster)
library(caret)
library(doMC)
library(doParallel)
names(dat_subset)
dim(dat_subset)
summary(dat_subset)
str(dat_subset)
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatdcv", number=5, repeats=5)
RFE_RF_model <- rfe(dat_subset[,-c(1:4)], dat_subset[,4], sizes=c(1:10), rfeControl=control2)
plot(RFE_RF_model, type=c("g", "o"))
predictors(RFE_RF_model)[1:4]
(RFE_RF_model2 <- rfe(dat_subset[predictors(RFE_RF_model)[1:4]], dat_subset[,4], sizes=c(1:6), rfeControl=control2) )
predictors(RFE_RF_model2)
stopCluster(cl = cl)

rm(cl)

names(COV84)
plot(COV84)
fm = as.formula(paste("COS30CM~", paste0(names(COV84)[c(24,22,6,2,3)],collapse = "+"))) 
fm
library(randomForest)
library(caret)

# Default 10-fold cross-validation
ctrl <- trainControl(method = "cv", savePred=T)

# Search for the best mtry parameter

install.packages("devtools")
library(devtools)
install_github('topepo/caret/pkg/caret',force=T)
library(caret)
library(randomForest)
rfmodel <- train(fm, data=dat_subset_sp@data, method = "rf", trControl = ctrl, 
                 importance=TRUE)
varImpPlot(rfmodel[11][[1]])
plot(rfmodel[11][[1]])
pred <- predict(COV84, rfmodel)
plot(pred)

writeRaster(pred, filename = "RF_5m_v3_12072018.tif",
            overwrite=TRUE)

library(Metrics)

start <- Sys.time()
validation <- data.frame(rmse=numeric(), r2=numeric())
#Sensitivity to the dataset
#Start a loop with 10 model realizations

for (i in 1:10){
    # We will build 10 models using random samples of 25%  
  smp_size <- floor(0.25 * nrow(dat_subset_sp))
  train_ind <- sample(seq_len(nrow(dat_subset_sp)), size = smp_size)
  train <- dat_subset_sp[train_ind, ]
  test <- dat_subset_sp[-train_ind, ]
  modn <- train(fm, data=train@data, method = "rf", 
                trControl = ctrl)
  pred <- stack(pred, predict(COV84, modn))
  test$pred <- extract(pred[[i+1]], test)
  # Store the results in a dataframe
  validation[i, 1] <- rmse(test$COS30CM, test$pred)
  validation[i, 2] <- cor(test$COS30CM, test$pred)^2
}
summary(validation)

sensitivity <- calc(pred[[-1]], sd)
plot(sensitivity, col=rev(topo.colors(10)), 
     main='Sensitivity based on 10 realizations using 25% samples')

prediction75 <- pred[[1]]
plot(prediction75, main='OCSKGM prediction based on 75% of data', 
     col=rev(topo.colors(10)))


library(quantregForest)
names(dat_subset_sp)

model <- quantregForest(y=dat_subset_sp@data$COS30CM, x=dat_subset_sp@data[,c(26,24,8,5,4)], 
                        ntree=250, keep.inbag=TRUE, 
                        mtry = as.numeric(rfmodel$bestTune))                        

library(snow)
beginCluster()
unc <- clusterR(COV84, predict, args=list(model=model,what=sd))

# OCSKGMlog prediction based in all available data
mean <- clusterR(COV84, predict, 
                 args=list(model=model, what=mean))
plot(mean, main='OCSKGM based in all data')
writeRaster(mean, "RF_PRED_SIB_5M_13072017.tif")

# The total uncertainty is the sum of sensitivity and model 
# uncertainty
unc <- unc + sensitivity
plot(unc,main="Inceridumbre COS30cm Sibundoy")
writeRaster(unc, "RF_UNC_SIB_5M_13072017.tif")
# Express the uncertainty in percent % (divide by the mean)
#Total_unc_Percent <- exp(unc)/exp(mean)
endCluster()

#======SUPPORT VECTOR MACHINE=====
names(dat_subset_sp)
selectedCovs <- cor(x = as.matrix(dat_subset_sp@data[,2]),
                    y = as.matrix(dat_subset_sp@data[,c(3:20)]))

# Print correlation results
selectedCovs

library(reshape)

x <- subset(melt(selectedCovs), value != 1 | value != NA)
x <- x[with(x, order(-abs(x$value))),]
x
idx <- as.character(x$X2[1:5])

dat2 <- dat_subset[c('COS30CM', idx)]
names(dat2)

COV <- COV84[[idx]]

# Selected covariates

names(COV)


###Categorical variables
dummyRaster <- function(rast){
  rast <- as.factor(rast)
  result <- list()
  for(i in 1:length(levels(rast)[[1]][[1]])){
    result[[i]] <- rast == levels(rast)[[1]][[1]][i]
    names(result[[i]]) <- paste0(names(rast), 
                                 levels(rast)[[1]][[1]][i])
  }
  return(stack(result))
}
names(COV84)


orden_dummy <- dummyRaster(COV84$Orden)
Forma_terreno_dummy <- dummyRaster(COV84$Forma_terreno)

COV <- stack(COV, orden_dummy,Forma_terreno_dummy)
names(COV)
COV <- COV[[-c(10,12,24,26,28,31,33)]]

names(dat_subset)

dat_subset_sp@data$Orden <- as.factor(dat_subset_sp@data$Orden)
dat_subset_sp@data$Forma_terreno <- as.factor(dat_subset_sp@data$Forma_terreno)

dat_orden_dummy <- model.matrix(~Orden -1, data = dat_subset_sp@data)
dat_orden_dummy <- as.data.frame(dat_orden_dummy)

dat_Forma_terreno_dummy <- model.matrix(~Forma_terreno -1, data = dat_subset_sp@data)
dat_Forma_terreno_dummy <- as.data.frame(dat_Forma_terreno_dummy)
dat_subset_sp@data <- cbind(dat_subset_sp@data, dat_orden_dummy , dat_Forma_terreno_dummy)
names(dat_subset_sp@data)



library(e1071)
library(caret)
names(COV)
names(dat_subset_sp@data)
names(dat_subset_sp@data[,c(2,4,5,6,20,12,17,19,8,27:42)])

# Test different values of epsilon and cost
tuneResult <- tune(svm, COS30CM ~.,  data = dat_subset_sp@data[,c(2,4,5,6,20,12,17,19,8,27:42)],
                                                       ranges = list(epsilon = seq(0.1,0.2,0.01),
                                 cost = c(1,5,10,15,20)))
plot(tuneResult)
tunedModel <- tuneResult$best.model
print(tunedModel)
names(COV)
OCSsvm <- predict(COV, tunedModel)
plot(OCSsvm)
values(OCSsvm)[values(OCSsvm) < 0]  <- NA
writeRaster(OCSsvm, filename = "SVM_PRED_SIB_5M_16072017_v2.tif",
            overwrite=TRUE)

summary(dat_subset$COS30CM)



# Variable importance in svm
# Code by: stackoverflow.com/questions/34781495
# Weight vectors
w <- t(tunedModel$coefs) %*% tunedModel$SV

# Weight
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  

w <- sort(w, decreasing = T)

print(w)


install.packages("snowfall")
install.packages("ranger")
library(caret)
library(doMC)
library(doParallel)
library(reshape)
library(Metrics)
library(caretEnsemble)
library(snowfall)
library(raster)
library(ranger)
library(sp)
library(gstat)
library(parallel)
library(quantregForest)

names(dat_subset_sp)
inTrain <- createDataPartition(y = dat_subset_sp@data[,2], p = .75, list = FALSE)
training <- dat_subset_sp@data[inTrain,]
testing <- dat_subset_sp@data[-inTrain,]

ls(getModelInfo())


set.seed(324)
ctrl <- trainControl(savePred=T, method="repeatedcv", number=3, repeats=3)
cl <- makeCluster(detectCores(), type='SOCK')
registerDoParallel(cl)
names(training) 
names(training[,c(2,6,20,12,17,19,27:42)])
names(COV)
names(COV[[-c(7,9,13,19,21,22,23,25,26,28,30,33)]])
COV_ens <- COV[[-c(7,9,13,19,21,22,23,25,26,28,30,33)]]
names(COV_ens)
training[names(COV_ens)]


(models <- caretList(training[names(COV_ens)], training[,2], trControl=ctrl ,
                     methodList=c("rf", "svmLinear")))

ens <- caretEnsemble(models)
stopCluster(cl = cl)
resamps <- resamples(models)

#ens <- readRDS('caretEnsemble-LAC.rds')

beginCluster()
(predEns <- clusterR(COV_ens, predict, args=list(model=ens, keepNA=FALSE)))
plot(predEns)
dir()
writeRaster(predEns, "ENS_PRED_SIB_5M_16072018_V2.tif",overwrite=TRUE) 
endCluster()

#uncertainty

testing$res <- (testing$COS30CM - predict(ens, testing[names(COV_ens)], keepNA=FALSE))

model <- quantregForest(y=testing$res, x=testing[names(COV_ens)], ntree=500, keep.inbag=TRUE)
beginCluster(6,type="SOCK")
#Estimate model uncertainty
unc <- clusterR(COV_ens,predict, args=list(model=model,what=sd))
plot(unc)

writeRaster(unc, "ENS_UNC_SIB_5M_16072018_V2.tif",overwrite=TRUE)



####GWRK

start <- Sys.time()
dat_subset_sp_28 <- spTransform(dat_subset_sp, CRS("+init=epsg:32618"))
COV_vf <- projectRaster(COV84, crs = CRS("+init=epsg:32618"), method='ngb')
print(Sys.time() - start)

library(reshape)
dat_subset_sp###32618
COV_vf###32618
plot(dat_subset_sp)
names(dat_subset_sp@data)
COR <- cor(as.matrix(dat_subset_sp@data[,2]), as.matrix(dat_subset_sp@data[,c(3:20)]),use="complete.obs")
COR
x <- subset(melt(COR), value != 1 | value != NA)
x <- x[with(x, order(-abs(x$value))),]
#as.character(x$X2[1:10])
x[1:6,]
names(dat_subset)
modelo.MLR <- lm(COS30CM ~ ANALYTICAL_HILLSHADING+NDVI_SIBUNDOY_CLIP+FLOW_ACCUMULATION+TOPOGRAPHIC_WETNESS_INDEX+VERTICAL_DISTANCE_TO_CHANNEL_NETWORK+RELATIVE_SLOPE_POSITION, data = dat_subset) 
summary(modelo.MLR)
modelo.MLR.step <- step(modelo.MLR, direction="both")
summary(modelo.MLR.step)
formula(modelo.MLR.step)

install.packages("spgwr",dependencies=T)
library(spgwr)
GWRbandwidth <- ggwr.sel(formula(modelo.MLR.step), data=dat_subset_sp,adapt=T) 
gwr.model = gwr(formula(modelo.MLR.step), data=dat_subset_sp, adapt=GWRbandwidth)
gwr.model

prediction <- predict(COV_vf,gwr.model)
image(gwr.model$SDF,'NDVI_SIBUNDOY_CLIP')

install.packages("GWmodel")
library(GWmodel)
gwr.predict(formula(modelo.MLR.step), data=dat_subset_sp,predictdata=COV_vf,GWRbandwidth)
str(gwr.model)
plot(gwr.model$SDF)

DM <- gw.dist(dp.locat=coordinates(dat_subset_sp),rp.locat=coordinates(COV_vf))
gwr.res <- gwr.basic(formula(modelo.MLR.step), data=dat_subset_sp, regression.points=COV_vf, bw=10, dMat=DM,kernel='gaussian')


COV84###WGS84
dat_sp84 <- dat_subset
coordinates(dat_sp84) <- ~ W_1 + N_1
str(dat_sp84)
dat_sp84
proy <- CRS("+proj=longlat +datum=WGS84")
dat_sp84@proj4string <- proy###WGS84





