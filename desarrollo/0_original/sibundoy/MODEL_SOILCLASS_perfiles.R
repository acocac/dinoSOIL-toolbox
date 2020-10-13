
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

dir()
COV84 <- stack("COV_WGS84_VF.tif")
names(COV84) <- readRDS("NAMES_COV_SIB_WGS84_VF.rds")
names(COV84)
COV84 <- COV84[[-24]]


#PARA OBSERVACIONES
sites <- data.frame(read_excel("E:\\SIBUNDOY\\Compilado Sibundoy\\8 BASE DE DATOS OBSERVACIONES DE CAMPO\\OBSERVACIONES_20160603_VF.xls", sheet = "SITES", na = "NA"))
names(sites)
dim(sites)
profiles <- data.frame(read_excel("E:\\SIBUNDOY\\Compilado Sibundoy\\8 BASE DE DATOS OBSERVACIONES DE CAMPO\\OBSERVACIONES_20160603_VF.xls", sheet = "VERTICALIZADO", na = "NA"))
names(profiles)
dim(profiles)
head(profiles)
summary(profiles)
profiles$soil_color <- with(profiles, munsell2rgb(hue, v,c))
data.rgb <- with(profiles, munsell2rgb(hue, v, c, return_triplets=TRUE))
profiles <- cbind(profiles,data.rgb)
depths(profiles) <- id ~ top + bottom
class(profiles)
profiles
names(sites)
names(profiles)
summary(sites)
site(profiles) <- sites
coordinates(profiles) <- ~ Este + Norte
str(profiles)
profiles@sp@proj4string
profiles@sp@proj4string <- CRS("+init=epsg:4326")
profiles@sp@proj4string


#VORONOI MAPS
names(profiles@horizons)
head(profiles$id_horz)
indx <- grep('_1$', profiles$id_horz)
profiles$hordiag <- factor(profiles$hordiag)
summary(profiles$hordiag)
txt <- data.frame(top=profiles$top[indx], bottom=profiles$bottom[indx],
                  x=coordinates(profiles)[,1],y=coordinates(profiles)[,2],
                  cl_txt=profiles$txt[indx],naf=profiles$naf[indx],
                  hcl=profiles$hcl[indx],h2o2=profiles$h2o2[indx],
                  dip=profiles$dip[indx],pH=profiles$ph[indx],esp=profiles$esp[indx])
txt
dim(txt)
txt_sp <- txt
coordinates(txt_sp) <- ~ x+y
proj4string(txt_sp) <- CRS("+init=epsg:4326")
txt_sp
vor <- dismo::voronoi(txt_sp)
plot(vor)

library(rgdal)
getwd()
lim <- readOGR(dsn="E:/SIBUNDOY/SIBUNDOY_covariables/COV_5M/LIMITE_SIBUNDOY_WGS84.shp")
vor@proj4string <- lim@proj4string
vor_txt <- intersect(vor, lim)
spplot(vor_txt,'dip')
vor_txt<- rasterize(vor_txt, COV84, 'dip')
plot(vor_txt)
writeRaster(vor_txt,"dip_H1_04092018.tif",overwrite=T)

dip <- raster("dip_H1_04092018.tif")
naf <- raster("naf_H1_04092018.tif")
text <- raster("cl_txt_H1_04092018.tif")
h2o2 <- raster("H2O2_H1_04092018.tif")
COV84 <- stack(COV84, dip, naf, text,h2o2)
names(COV84)[24:27] <- c("dip","naf","text","h2o2")
names(COV84)

#####MATRIZ DE REGRESION
names(profiles@site)
coordinates(profiles)
dat_subset <- data.frame(coordinates(profiles),id=profiles@site$id,orden=profiles@site$soilorder,suborden=profiles@site$suborder,grangrupo=profiles@site$greatgroup,subgrupo=profiles@site$subgroup)

dim(dat_subset)
summary(dat_subset)
names(dat_subset)
dat_subset$id <- as.factor(dat_subset$id)
str(dat_subset)


#### Juntamos 1_covariables con los datos  ####
# Convert to spatial points df and project
library(sp)
dat_subset_sp <- dat_subset
coordinates(dat_subset_sp) <- ~ Este + Norte
str(dat_subset_sp)
dat_subset_sp
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy

start <- Sys.time()
dat_subset <- cbind(dat_subset, extract(COV84, dat_subset_sp))
summary(dat_subset)
print(Sys.time() - start)


write.csv(dat_subset, 'RegMatrix_VF_Antes_observaciones.csv')
dat_subset <- dat_subset[complete.cases(dat_subset[,]),]
write.csv(dat_subset, 'RegMatrix_VF_observaciones.csv')
dim(dat_subset)

#importacia de 1_covariables para orden
dat_subset <- read.csv('RegMatrix_VF_observaciones.csv',sep=";")
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
dat_subset$ORDEN <- factor(dat_subset$ORDEN)
dat_subset$COBERTURA <- factor(dat_subset$COBERTURA)
dat_subset$DIP <- factor(dat_subset$DIP)
dat_subset$NAF <- factor(dat_subset$NAF)
dat_subset$TEXT <- factor(dat_subset$TEXT)
dat_subset$H2O2 <- factor(dat_subset$H2O2)
str(dat_subset)

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatdcv", number=5, repeats=5)
(RFE_RF_model <- rfe(dat_subset[-c(1,2,3,4,5,6,7,8)], dat_subset[,5], sizes=c(1:6), rfeControl=control2))

plot(RFE_RF_model, type=c("g", "o"))
predictors(RFE_RF_model)[1:4]
(RFE_RF_model2 <- rfe(dat_subset[predictors(RFE_RF_model)[1:4]], dat_subset[,4], sizes=c(1:6), rfeControl=control2) )
predictors(RFE_RF_model2)
stopCluster(cl = cl)




#PARA PERFILES
#======RANDOM FOREST=====
########IMPORTANCIA DE LAS COVARIABLES

dat_subset_p <- read.csv("rmat.csv",sep=";")
TAX <- readOGR(dsn="E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\PERFILES_VF.shp")
head(TAX)
names(TAX)
dat_subset <- join(x=dat_subset,y=TAX@data,by="PERFIL")
dim(dat_subset)
names(dat_subset_p)
names(dat_subset)
summary(dat_subset)

dat_subset[,c(30:35,37:49)] <- NULL
dat_subset[,4] <- NULL
dat_subset[,30] <- NULL

dim(dat_subset)
dat_subset$Orden <- factor(dat_subset$Orden)
dat_subset$Cobertura <- factor(dat_subset$Cobertura)
dat_subset$Paisaje <- factor(dat_subset$Paisaje)
dat_subset$Forma_terreno <- factor(dat_subset$Forma_terreno)
dat_subset$Mat_parental <- factor(dat_subset$Mat_parental)
dat_subset$Paisaje <- factor(dat_subset$Paisaje)
dat_subset$Tipo_relieve <- factor(dat_subset$Tipo_relieve)
str(dat_subset)

names(dat_subset)
getwd()
##VARIABLE IMPORTANCE 

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5)
(RFE_RF_model <- rfe(dat_subset[-c(1,2,3,28,29,30)], dat_subset[,27], sizes=c(1:6), rfeControl=control2) )
plot(RFE_RF_model, type=c("g", "o"))
predictors(RFE_RF_model)[1:3]
(RFE_RF_model2 <- rfe(dat_subset[predictors(RFE_RF_model)[1:3]], dat_subset[,28], sizes=c(1:6), rfeControl=control2) )
stopCluster(cl = cl)

names(COV84)
plot(COV84)
fm = as.formula(paste("Orden~", paste0(names(COV84)[c(23,20)],collapse = "+"))) 
fm
library(randomForest)
library(caret)

# Default 10-fold cross-validation
ctrl <- trainControl(method = "cv", savePred=T)

# Search for the best mtry parameter

library(caret)
library(randomForest)
dat_subset_sp <- dat_subset
coordinates(dat_subset_sp) <- ~ W_1 + N_1
str(dat_subset_sp)
names(dat_subset_sp)
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy

rfmodel <- train(fm, data=dat_subset_sp@data, method = "rf", trControl = ctrl, 
                 importance=TRUE)
str(rfmodel)
rfmodel$pred[,1]
xtab <- table(rfmodel$pred[,1], rfmodel$pred[,2])
con.mat <- confusionMatrix(xtab)
con.mat

summary(dat_subset$Mat_parental)

varImpPlot(rfmodel[11][[1]])
plot(rfmodel[11][[1]])
pred <- predict(COV84, rfmodel)
plot(pred)

writeRaster(pred, filename = "RF_5m_PRED_SUBORDEN_03092018.tif",
            overwrite=TRUE)







ls(getModelInfo())
rlogmodel <- train(fm, data=dat_subset_sp@data, method = "regLogistic", trControl = ctrl, 
                 importance=TRUE)
str(rfmodel)
rfmodel$pred[,1]
xtab <- table(rfmodel$pred[,1], rfmodel$pred[,2])
con.mat <- confusionMatrix(xtab)
con.mat

varImpPlot(rlogmodel[11][[1]])
plot(rlogmodel[11][[1]])
pred <- predict(COV84, rlogmodel)
plot(pred)


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


ls(getModelInfo())

#======SUPPORT VECTOR MACHINE=====
names(dat_subset_sp)
selectedCovs <- cor(x = as.matrix(dat_subset_sp@data[,29]),
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




