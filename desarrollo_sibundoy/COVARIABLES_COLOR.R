
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


##PARA OBSERVACIONES
#Cargando covariables disponibles
dir()
COV84 <- stack("COV_SIB_WGS84_CAJ_VF.tif")
names(COV84) <- readRDS("NAMES_COV_SIB_WGS84_CAJ_VF.rds")
names(COV84)
#ADICIONANDO A LAS COVARIABLES DE GEOMORFOLOGÍA Y EL MATERIAL PARENTAL

#####Covariable tipo de relieve#############
start <- Sys.time()
polyg_geom <- readOGR(dsn="GEOMORF_MATPAR_SIB_WGS84.shp")
summary(polyg_geom)
polyg_geom_geog <- spTransform (polyg_geom, CRS=projection(COV84))
polyg_geom_geog_rasterized <- rasterize(polyg_geom_geog, COV84, "T_RELIEVE")
polyg_geom_geog_rasterized <- resample(polyg_geom_geog_rasterized,COV84,method="bilinear")
names(polyg_geom_geog_rasterized) <- 'T_Relieve'
COV84 <- stack(COV84, polyg_geom_geog_rasterized)
names(COV84)
COV84[[21]]


#####Covariable forma del terreno#############

polyg_geom <- readOGR(dsn="GEOMORF_MATPAR_SIB_WGS84.shp")
summary(polyg_geom)
polyg_geom_geog <- spTransform (polyg_geom, CRS=projection(COV84))
polyg_geom_geog_rasterized <- rasterize(polyg_geom_geog, COV84, "F_TERRENO")
polyg_geom_geog_rasterized <- resample(polyg_geom_geog_rasterized,COV84,method="bilinear")
names(polyg_geom_geog_rasterized) <- 'F_TERRENO'
COV84 <- stack(COV84, polyg_geom_geog_rasterized)
names(COV84)
COV84[[22]]


#####Covariable material parental#############

polyg_geom <- readOGR(dsn="GEOMORF_MATPAR_SIB_WGS84.shp")
summary(polyg_geom)
polyg_geom_geog <- spTransform (polyg_geom, CRS=projection(COV84))
polyg_geom_geog_rasterized <- rasterize(polyg_geom_geog, COV84, "MATERIALES")
polyg_geom_geog_rasterized <- resample(polyg_geom_geog_rasterized,COV84,method="bilinear")
names(polyg_geom_geog_rasterized) <- 'MAT_PARENTAL'
COV84 <- stack(COV84, polyg_geom_geog_rasterized)
names(COV84)
COV84[[20]]
print(Sys.time() - start)




###Trabajando con el color
sites <- data.frame(read_excel("E:\\SIBUNDOY\\Compilado Sibundoy\\8 BASE DE DATOS OBSERVACIONES DE CAMPO\\OBSERVACIONES_20160603_VF.xls", sheet = "SITES", na = "NA"))
names(sites)
dim(sites)
str(sites)
sites$id <- factor(sites$id)
profiles <- data.frame(read_excel("E:\\SIBUNDOY\\Compilado Sibundoy\\8 BASE DE DATOS OBSERVACIONES DE CAMPO\\OBSERVACIONES_20160603_VF.xls", sheet = "VERTICALIZADO", na = "NA"))




library(munsellinterpol)
color <- data.frame(id_horz=profiles$id_horz,color_vf=profiles$col_vf)
color <- na.omit(color)

color_munsell <- as.character(color[,2])
color <- cbind(color,MunsellToLuv(as.character(color[,2])))

color$Luv <- color$L+color$u+color$v

RI_1 <- function(color_munsell){
  lab <- MunsellToLab(color_munsell)
  L <- lab[1]
  a <- lab[2]
  b <- lab[3]
  indice <- (L*((a^2 +b^2)^0.5)*(10^8.2))/(b*(L^6))
  return(indice[1])
}

fin  <- c()
for(i in 1:length(color_munsell)){
  temp <- RI_1(color_munsell[i])
  fin <- rbind(fin,temp)
}
fin
color_2 <- data.frame(id_horz=color[,1], red_ind=fin[,1])
names(color_2)
profiles <- join(profiles,color_2,"id_horz", type="left")
head(profiles)
names(profiles)
summary(profiles)
tail(profiles)



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
names(profiles)
###Interpolando componentes del color RGB e índice de enrojecimiento
indx1 <- grep('_2$', profiles$id_horz)#Primer horizonte
dat <- data.frame(id=profiles$id[indx1],top=profiles$top[indx1], bottom=profiles$bottom[indx1],
                  x=coordinates(profiles)[,1],y=coordinates(profiles)[,2], col=profiles$col[indx1],
                  col_vf=profiles$col_vf[indx1],HUE=profiles$hue[indx1],V=profiles$v[indx1],
                  C=profiles$c[indx1],r=profiles$r[indx1],g=profiles$g[indx1],
                  b=profiles$b[indx1],SOIL_COLOR=profiles$soil_color[indx1],L=profiles$L[indx1],u=profiles$u[indx1],
                  v=profiles$v[indx1], Luv=profiles$Luv[indx1],red_ind=profiles$red_ind[indx1])


names(dat) 

dat <- dat[complete.cases(dat[,]),]
dat_sp <- dat
coordinates(dat_sp) <- ~ x+y
proy <- CRS("+proj=longlat +datum=WGS84")
dat_sp@proj4string <- proy
dat_sp <- spTransform(dat_sp,CRS("+init=epsg:32618"))
coordinates(dat_sp)
class(dat_sp)
dim(dat_sp)
names(dat_sp)


##INterpolacion Kriging Ordinario
getwd()
COV <- stack("COV_SIB_WGS84_CAJ_REACCIONES_VF.tif")
names(COV) <- readRDS("NAMES_COV_SIB_WGS84_CAJ_REACCIONES_VF.rds")
names(COV)
plot(COV[[1]])
COV_PROY <- projectRaster(COV[[1]], crs = CRS("+init=epsg:32618"),
                      method='ngb')
COV_SPG <- as(COV_PROY, "SpatialGridDataFrame")
plot(COV_SPG)
proj4string(dat_sp) <- CRS("+init=epsg:32618")
names(dat_sp)
library(automap)
kriging_result = autoKrige(L~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)

kriging_result_2 = autoKrige(u~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)

kriging_result_3 = autoKrige(v~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)

kriging_result_4 = autoKrige(Luv~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)

kriging_result_5 = autoKrige(r~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)

kriging_result_6 = autoKrige(r~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)

kriging_result_7 = autoKrige(b~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)

kriging_result_8 = autoKrige(red_ind~1, input_data=dat_sp,new_data=COV_SPG,verbose = T)


OKpred_1 <- raster(kriging_result$krige_output[1])
OKpredsd_1 <- raster(kriging_result$krige_output[3])


OKpred_2 <- raster(kriging_result_2$krige_output[1])
OKpredsd_2 <- raster(kriging_result_2$krige_output[3])

OKpred_3 <- raster(kriging_result_3$krige_output[1])
OKpredsd_3 <- raster(kriging_result_3$krige_output[3])

OKpred_4 <- raster(kriging_result_4$krige_output[1])
OKpredsd_4 <- raster(kriging_result_4$krige_output[3])

OKpred_5 <- raster(kriging_result_5$krige_output[1])
OKpredsd_5 <- raster(kriging_result_5$krige_output[3])

OKpred_6 <- raster(kriging_result_6$krige_output[1])
OKpredsd_6 <- raster(kriging_result_6$krige_output[3])

OKpred_7 <- raster(kriging_result_7$krige_output[1])
OKpredsd_7 <- raster(kriging_result_7$krige_output[3])

OKpred_8 <- raster(kriging_result_8$krige_output[1])
OKpredsd_8 <- raster(kriging_result_8$krige_output[3])


writeRaster(OKpred_1,"OK_PRED_L_H2.tif",overwrite=T)
writeRaster(OKpredsd_1,"OK_PREDSD_L_H2.tif",overwrite=T)

writeRaster(OKpred_2,"OK_PRED_u_H2.tif",overwrite=T)
writeRaster(OKpredsd_2,"OK_PREDSD_u_H2.tif",overwrite=T)

writeRaster(OKpred_3,"OK_PRED_v_H2.tif",overwrite=T)
writeRaster(OKpredsd_3,"OK_PREDSD_v_H2.tif",overwrite=T)

writeRaster(OKpred_4,"OK_PRED_Luv_H2.tif",overwrite=T)
writeRaster(OKpredsd_4,"OK_PREDSD_Luv_H2.tif",overwrite=T)

writeRaster(OKpred_5,"OK_PRED_r_H2.tif",overwrite=T)
writeRaster(OKpredsd_5,"OK_PREDSD_r_H2.tif",overwrite=T)

writeRaster(OKpred_6,"OK_PRED_g_H2.tif",overwrite=T)
writeRaster(OKpredsd_6,"OK_PREDSD_g_H2.tif",overwrite=T)

writeRaster(OKpred_7,"OK_PRED_b_H2.tif",overwrite=T)
writeRaster(OKpredsd_7,"OK_PREDSD_b_H2.tif",overwrite=T)

writeRaster(OKpred_8,"OK_PRED_RI_H2.tif",overwrite=T)
writeRaster(OKpredsd_8,"OK_PREDSD_RI_H2.tif",overwrite=T)


kr.cv = autoKrige.cv(L~1, input_data=dat_sp, nfold = 10)
kr.cv_2= autoKrige.cv(u~1, input_data=dat_sp, nfold = 10)
kr.cv_3 = autoKrige.cv(v~1, input_data=dat_sp, nfold = 10)
kr.cv_4 = autoKrige.cv(Luv~1, input_data=dat_sp, nfold = 10)
kr.cv_5 = autoKrige.cv(r~1, input_data=dat_sp, nfold = 10)
kr.cv_6 = autoKrige.cv(g~1, input_data=dat_sp, nfold = 10)
kr.cv_7 = autoKrige.cv(b~1, input_data=dat_sp, nfold = 10)
kr.cv_8 = autoKrige.cv(red_ind~1, input_data=dat_sp, nfold = 10)
summary(kr.cv)
summary(kr.cv_2)
summary(kr.cv_3)
summary(kr.cv_4)
summary(kr.cv_5)
summary(kr.cv_6)
summary(kr.cv_7)
summary(kr.cv_8)

##Interpolación IDW
library(gstat)
lim <- readOGR(dsn="E:/SIBUNDOY/SIBUNDOY_covariables/COV_5M/LIMITE_SIBUNDOY_WGS84.shp")
lim <- spTransform(lim,CRS("+init=epsg:32618"))


idw_r<-idw(formula=r~1, locations=dat_sp, newdata=COV_SPG)
idw_r <- raster(idw_r, 'var1.pred')
plot(idw_r)
idw_r <- idw_r*255
writeRaster(idw_r,"idw_r.tif",overwrite=T)
#idw_r <- mask(idw_r, lim)


idw_g<-idw(formula=g~1, locations=dat_sp, newdata=COV_SPG)
idw_g <- raster(idw_g, 'var1.pred')
plot(idw_g)
idw_g <- idw_g*255
writeRaster(idw_g,"idw_g_H2.tif",overwrite=T)
#idw_g <- mask(idw_g, lim)

idw_b<-idw(formula=b~1, locations=dat_sp, newdata=COV_SPG)
idw_b <- raster(idw_b, 'var1.pred')
plot(idw_b)
idw_b <- idw_b*255
writeRaster(idw_b,"idw_b_H2.tif",overwrite=T)
#idw_b <- mask(idw_b, lim)




##OTROS INDICES DE COLOR
OK_R <- raster("OK_PRED_R_Clip.tif")
OK_G <- raster("OK_PRED_G_Clip.tif")
OK_B <- raster("OK_PRED_B_Clip.tif")

IDW_R <- raster("idw_r_clip.tif")
IDW_G <- raster("idw_g_clip.tif")
IDW_B <- raster("idw_b_clip.tif")

OK_CI <- (OK_R-OK_G)/(OK_R+OK_G)
IDW_CI <- (IDW_R-IDW_G)/(IDW_R+IDW_G)
par(mfrow=c(5,2))
plot(OK_CI)
plot(IDW_CI)

OK_RI <- (OK_R^2)/(OK_B*(OK_G^3))
IDW_RI<- (IDW_R^2)/(IDW_B*(IDW_G^3))
par(mfrow=c(1,2))
plot(OK_RI)
plot(IDW_RI)

OK_SI <- (OK_R-OK_B)/(OK_R+OK_B)
IDW_SI <- (IDW_R-IDW_B)/(IDW_R+IDW_B)
par(mfrow=c(1,2))
plot(OK_SI)
plot(IDW_SI)

OK_BI <- sqrt(((OK_R^2)+(OK_G^2)+(OK_B^2))/3)
IDW_BI <- sqrt(((IDW_R^2)+(IDW_G^2)+(IDW_B^2))/3)
par(mfrow=c(1,2))
plot(OK_BI)
plot(IDW_BI)

OK_HI <- (2*(OK_R-OK_G-OK_B))/(OK_G-OK_B)
IDW_HI <- (2*(IDW_R-IDW_G-IDW_B))/(IDW_G-IDW_B)
par(mfrow=c(1,2))
plot(OK_HI)
plot(IDW_HI)

#plot(r)
#points(p)

###======NUMERICAL VARIABLES- ORDINARY KRIGING=====

# MORAN INDEX
library(spdep)
names(dat_sp)
proj4string(dat_sp) <- CRS("+init=epsg:3116")
dat_sp <- spTransform(dat_sp,CRS("+init=epsg:3116"))
cord <- coordinates(dat_sp)
k1 <- knn2nb(knearneigh(cord))
all.linked <- max(unlist(nbdists(k1, cord)))
gri <- dnearneigh(cord,0,all.linked)
lw <- nb2listw(gri, style = "W")

###Componente RED r
i.moran <- moran.mc(dat_sp$r, list=lw, nsim=999)
i.moran

###Componente GREEN g
i.moran <- moran.mc(dat_sp$g, list=lw, nsim=999)
i.moran

###Componente GREEN g
i.moran <- moran.mc(dat_sp$b, list=lw, nsim=999)
i.moran

###Indice de enrojecimiento
i.moran <- moran.mc(dat_sp$REDIND, list=lw, nsim=999)
i.moran

###Semiautomatic - using GSTAT and GEOR packages
library(geoR)
dat_sp2 <- data.frame(x=coordinates(dat_sp)[,1],y=coordinates(dat_sp)[,2],
                      RI=dat_sp$REDIND, r=dat_sp$r,g=dat_sp$g,b=dat_sp$b)
dat_sp2
geo = as.geodata(dat_sp2, coords.col = 1:2, data.col = 4)
plot(geo, scatter3d = TRUE)

(var1 = variog(geo))

var2 <- variog(geo)
plot(var2,ylim=c(0,0.018))

plot(variog4(geo))### max.dist maxima distancia a la cual calcular la semivarianza

x11()
(ev=eyefit(var2))
ev
ev[[1]]


mod1cub=variofit(var2,ini=ev[[1]]$cov.pars,nugget=ev[[1]]$nugget,fix.nugget=F,cov.model=ev[[1]]$cov.model,weights="equal")
mod2cub=variofit(var2,ini=ev[[1]]$cov.pars,nugget=ev[[1]]$nugget,fix.nugget=F,cov.model=ev[[1]]$cov.model,weights="npairs")
mod3cub=variofit(var2,ini=ev[[1]]$cov.pars,nugget=ev[[1]]$nugget,fix.nugget=F,cov.model=ev[[1]]$cov.model,weights="cressie")
mod4cub=likfit(geo,ini=ev[[1]]$cov.pars,nugget=ev[[1]]$nugget,fix.nugget=F,cov.model=ev[[1]]$cov.model,lik.method = "ML")
mod5cub=likfit(geo,ini=ev[[1]]$cov.pars,nugget=ev[[1]]$nugget,fix.nugget=F,cov.model=ev[[1]]$cov.model,lik.method = "REML")


plot(var2, main = expression(paste("Cubic estimated ", tau^2)), ylim = c(0,350))
lines(mod1cub, max.dist = 15428.47, col = 1)
lines(mod2cub, max.dist = 15428.47, col = 2)
lines(mod3cub, max.dist = 15428.47, col = 3)
lines(mod4cub, max.dist = 15428.47, col = 4)
lines(mod5cub, max.dist = 15428.47, col = 5)
legend(0.02, 350, legend = c("OLS", "WLS - npairs", "WLS - cressie", "ML", "REML"),col = 1:5, lty = c(1, 1, 2, 2), lwd = c(1, 2, 1, 2), cex = 0.7)

library(gstat)
ve.fit1cub <- as.vgm.variomodel(mod1cub)
ve.fit2cub <- as.vgm.variomodel(mod2cub)
ve.fit3cub <- as.vgm.variomodel(mod3cub)
ve.fit4cub <- as.vgm.variomodel(mod4cub)
ve.fit5cub <- as.vgm.variomodel(mod5cub)

cvolsexp <- krige.cv(REDIND ~ 1, dat_sp, ve.fit1cub, maxdist = 15428.47)
cvwlsnpairsexp <- krige.cv(pH ~ 1, txt_sp, ve.fit2exp, maxdist = 15428.47)
cvwlsncressieexp <- krige.cv(pH ~ 1, txt_sp, ve.fit3exp, maxdist = 15428.47)
cvmlexp <- krige.cv(pH ~ 1, txt_sp, ve.fit4exp, maxdist = 15428.47)
cvremlexp <- krige.cv(pH ~ 1, txt_sp, ve.fit5exp, maxdist = 15428.47)


#Estructurando matriz de regresión
setwd("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\COV_COLOR")
COV <- stack("COV_VF.tif")
names(COV) <- readRDS("NAMES_COV_VF_COLOR.rds")
names(COV)
COV
plot(COV[[30]])



names(profiles@horizons)
head(profiles$id_horz)
indx1 <- grep('_1$', profiles$id_horz)#Primer horizonte
indx2 <- grep('_2$', profiles$id_horz)#Primer horizonte
dat <- data.frame(id=profiles$id[indx1],top=profiles$top[indx1], bottom=profiles$bottom[indx1],
                  x=coordinates(profiles)[,1],y=coordinates(profiles)[,2],
                  cl_txt_1=profiles$txt[indx1],naf_1=profiles$naf[indx1],
                  hcl_1=profiles$hcl[indx1],h2o2_1=profiles$h2o2[indx1],
                  dip_1=profiles$dip[indx1],pH_1=profiles$ph[indx1],esp_1=profiles$esp[indx1],
                  cl_txt_2=profiles$txt[indx2],naf_2=profiles$naf[indx2],
                  hcl_2=profiles$hcl[indx2],h2o2_2=profiles$h2o2[indx2],
                  dip_2=profiles$dip[indx2],pH_2=profiles$ph[indx2],esp_2=profiles$esp[indx2])
dat

names(profiles@site)
coordinates(profiles)
dat_subset <- data.frame(coordinates(profiles),id=profiles@site$id,orden=profiles@site$soilorder,suborden=profiles@site$suborder,grangrupo=profiles@site$greatgroup,subgrupo=profiles@site$subgroup)
dat_subset <- join(dat_subset, dat, type="inner")

dim(dat_subset)
summary(dat_subset)
names(dat_subset)
dat_subset$id <- as.factor(dat_subset$id)
str(dat_subset)


#### Juntamos covariables con los datos  ####
# Convert to spatial points df and project
library(sp)
dat_subset_sp <- dat_subset
coordinates(dat_subset_sp) <- ~ Este + Norte
str(dat_subset_sp)
dat_subset_sp
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy

start <- Sys.time()
dat_subset <- cbind(dat_subset, extract(COV, dat_subset_sp))
summary(dat_subset)
names(dat_subset)
print(Sys.time() - start)

write.csv(dat_subset, 'RegMatrix_VF_Antes_observaciones.csv')
dim(dat_subset)
dat_subset <- dat_subset[complete.cases(dat_subset[,]),]
dim(dat_subset)
write.csv(dat_subset, 'RegMatrix_VF_observaciones.csv')
dim(dat_subset)

#importacia de covariables para orden
dat_subset <- read.csv('RegMatrix_VF_observaciones.csv',sep=",")
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

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=rfFuncs, method="repeatdcv", number=10, repeats=10)
(RFE_RF_model <- rfe(dat_subset[-c(1:26,32)], dat_subset[,5], sizes=c(1:10), rfeControl=control2))
plot(RFE_RF_model, type=c("g", "o"))
predictors(RFE_RF_model)[1:5]
(RFE_RF_model2 <- rfe(dat_subset[predictors(RFE_RF_model)[1:5]], dat_subset[,33], sizes=c(1:6), rfeControl=control2) )
predictors(RFE_RF_model2)
stopCluster(cl = cl)


names(dat_subset)
names(COV)
plot(COV84[[2]])
fm = as.formula(paste("orden~", paste0(names(COV)[c(9,3,2,21,30)],collapse = "+"))) 
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
pred <- predict(COV, rfmodel,filename = "RF_5m_PRED_orden_17092018.tif",
                format = "GTiff", overwrite = T)
plot(pred)




##REGRESIÓN LOGÍSTICA MULTINOMIAL
library(nnet)
set.seed(8)
training <- sample(nrow(dat_subset), 0.7 * nrow(dat_subset))
rlmodel <- multinom(fm, data = dat_subset[training, ])
summary(rlmodel)
probs.rlmodel<- fitted(rlmodel)
pred.rlmodel <- predict(rlmodel)

str(rlmodel)

xtab <- table(dat_subset$naf_1[training],pred.rlmodel)
con.mat <- confusionMatrix(xtab)
con.mat

V.pred.rlmodel <- predict(rlmodel, newdata = dat_subset[-training, ])
xtab.v <- table(dat_subset$NAF_1[-training],V.pred.rlmodel)
con.mat.v <- confusionMatrix(xtab)
con.mat.v

map.RLMN.c <- predict(COV84, rlmodel, type = "class",filename = "RLMN_5m_PRED_DIP_1_CAJ_10092018.tif",
                      format = "GTiff", overwrite = T)

plot(map.RLMN.c)

map.RLMN.p4 <- predict(COV84, rlmodel, type = "probs", index = 4,
                      filename = "RLMN_5m_PROB4_NAF_1_CAJ_10092018.tif", format = "GTiff", overwrite = T)

plot(map.RLMN.p4)


map.RLMN.p1 <- predict(COV84, rlmodel, type = "probs", index = 1,
                       filename = "RLMN_5m_PROB1_NAF_1_CAJ_10092018.tif", format = "GTiff", overwrite = T)

plot(map.RLMN.p1)


map.RLMN.p3 <- predict(COV84, rlmodel, type = "probs", index = 3,
                       filename = "RLMN_5m_PROB3_NAF_1_CAJ_10092018.tif", format = "GTiff", overwrite = T)

plot(map.RLMN.p3)



#####MODELANDO TIPO DE SUELO

COV84
RF_NAF_C <- raster("RF_5m_PRED_NAF_1_CAJ_10092018.tif")
RL_NAF_C <- raster("RLMN_5m_PRED_NAF_1_CAJ_10092018.tif")
RL_NAF_P4 <- raster("RLMN_5m_PROB4_NAF_1_CAJ_06092018.tif")
RL_NAF_P1 <- raster("RLMN_5m_PROB1_NAF_1_CAJ_06092018.tif")
RL_NAF_P3 <- raster("RLMN_5m_PROB3_NAF_1_CAJ_06092018.tif")

RF_DIP_C <- raster("RF_5m_PRED_DIP_1_CAJ_10092018.tif")
RL_DIP_C <- raster("RLMN_5m_PRED_DIP_1_CAJ_06092018.tif")
RL_DIP_P4 <-  raster("RLMN_5m_PROB4_DIP_1_CAJ_06092018.tif")
RL_DIP_P1 <- raster("RLMN_5m_PROB1_DIP_1_CAJ_06092018.tif")
RL_DIP_P3 <-  raster("RLMN_5m_PROB3_DIP_1_CAJ_06092018.tif")

RF_H2O2_C <- raster("RF_5m_PRED_H2O2_1_CAJ_06092018.tif")
RL_H2O2_C <- raster("RLMN_5m_PRED_H2O2_1_CAJ_06092018.tif")
RL_H2O2_P4 <- raster("RLMN_5m_PROB4_H2O2_1_CAJ_06092018.tif")
RL_H2O2_P1 <- raster("RLMN_5m_PROB1_H2O2_1_CAJ_06092018.tif")
RL_H2O2_P2 <- raster("RLMN_5m_PROB2_H2O2_1_CAJ_06092018.tif")

COV84 <- stack(COV84, 
      RF_NAF_C,
      RL_NAF_C,
      RF_DIP_C)


names(COV84)
writeRaster(COV84, "COV_SIB_WGS84_CAJ_REACCIONES_VF.tif",format="GTiff",overwrite=T)
saveRDS(names(COV84),"NAMES_COV_SIB_WGS84_CAJ_REACCIONES_VF.rds")


dat_subset_sp <- dat_subset
coordinates(dat_subset_sp) <- ~ Este + Norte
str(dat_subset_sp)
dat_subset_sp
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy

start <- Sys.time()
dat_subset <- cbind(dat_subset, extract(COV84, dat_subset_sp))
summary(dat_subset)
names(dat_subset)
print(Sys.time() - start)

names(dat_subset)
dat_subset <- dat_subset[,-c(49:70)]
dim(dat_subset)
write.csv(dat_subset, 'RegMatrix_VF_Antes_observaciones_reacciones.csv')
dim(dat_subset)
dat_subset <- dat_subset[complete.cases(dat_subset[,]),]
dim(dat_subset)
write.csv(dat_subset, 'RegMatrix_VF_observaciones_reacciones.csv')
dim(dat_subset)
names(dat_subset)
colnames(dat_subset)[c(49:51)] <- c("RF_NAF_C",
                                  "RL_NAF_C",
                                  "RF_DIP_C")

names(COV84)[23:25]  <- c("RF_NAF_C",
  "RL_NAF_C",
  "RF_DIP_C")

colnames(dat_subset)
names(dat_subset)
write.csv(dat_subset, 'RegMatrix_VF_observaciones_reacciones.csv')

#importacia de covariables para orden
dat_subset <- read.csv('RegMatrix_VF_observaciones_reacciones.csv',sep=",")
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
dat_subset$DIP_1 <- factor(dat_subset$DIP_1)
dat_subset$NAF_1 <- factor(dat_subset$NAF_1)
dat_subset$H2O2_1 <- factor(dat_subset$H2O2_1)
dat_subset$DIP_2 <- factor(dat_subset$DIP_2)
dat_subset$NAF_2 <- factor(dat_subset$NAF_2)
dat_subset$H2O2_2 <- factor(dat_subset$H2O2_2)
str(dat_subset)

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=nbFuncs, method="repeatdcv", number=5, repeats=5)
(RFE_RF_model <- rfe(dat_subset[-c(1:46)], dat_subset[,6], sizes=c(1:6), rfeControl=control2) )
plot(RFE_RF_model, type=c("g", "o"))
predictors(RFE_RF_model)[1:3]
(RFE_RF_model2 <- rfe(dat_subset[predictors(RFE_RF_model)[1:5]], dat_subset[,33], sizes=c(1:6), rfeControl=control2) )
predictors(RFE_RF_model2)
stopCluster(cl = cl)


names(dat_subset)
names(COV84)
 plot(COV84[[1]])
fm = as.formula(paste("orden~", paste0(names(COV84)[c(21,25,22,20,23)],collapse = "+"))) 
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
ls(getModelInfo())
rfmodel <- train(fm, data=dat_subset_sp@data, method = "nnet", trControl = ctrl, 
                 importance=TRUE)
rfmodel$pred[,1]
xtab <- table(rfmodel$pred[,1], rfmodel$pred[,2])
con.mat <- confusionMatrix(xtab)
con.mat


varImpPlot(rfmodel[11][[1]])
plot(rfmodel[11][[1]])
pred <- predict(COV84, rfmodel,filename = "RF_5m_PRED_orden_CAJ_10092018.tif",
                format = "GTiff", overwrite = T)
plot(pred)

profiles




##REGRESIÓN LOGÍSTICA MULTINOMIAL
library(nnet)
set.seed(14)
training <- sample(nrow(dat_subset), 0.7 * nrow(dat_subset))
rlmodel <- multinom(fm, data = dat_subset[training, ])
summary(rlmodel)
probs.rlmodel<- fitted(rlmodel)
pred.rlmodel <- predict(rlmodel)

str(rlmodel)

xtab <- table(dat_subset$orden[training],pred.rlmodel)
con.mat <- confusionMatrix(xtab)
con.mat

V.pred.rlmodel <- predict(rlmodel, newdata = dat_subset[-training, ])
xtab.v <- table(dat_subset$suborden[-training],V.pred.rlmodel)
con.mat.v <- confusionMatrix(xtab)
con.mat.v

map.RLMN.c <- predict(COV84, rlmodel, type = "class",filename = "RLMN_5m_PRED_orden_CAJ_06092018.tif",
                      format = "GTiff", overwrite = T)

plot(map.RLMN.c)
library(rasterVis)
map.RLMN.c <- as.factor(map.RLMN.c)
rat <- levels(map.RLMN.c)[[1]]
rat[["orden"]] <- c("Andisol", "Histosol", "Inceptisol")
levels(map.RLMN.c) <- rat
# plot
area_colors <- c("#FF0000", "#73DFFF", "#FFEBAF")
levelplot(map.RLMN.c, col.regions = area_colors, xlab = "", ylab = "")


map.RLMN.p4 <- predict(COV84, rlmodel, type = "probs", index = 4,
                       filename = "RLMN_5m_PROB4_orden_CAJ_06092018.tif", format = "GTiff", overwrite = T)

plot(map.RLMN.p4)


map.RLMN.p1 <- predict(COV84, rlmodel, type = "probs", index = 1,
                       filename = "RLMN_5m_PROB1_orden_CAJ_06092018.tif", format = "GTiff", overwrite = T)

plot(map.RLMN.p1)


map.RLMN.p3 <- predict(COV84, rlmodel, type = "probs", index = 3,
                       filename = "RLMN_5m_PROB3_orden_CAJ_06092018.tif", format = "GTiff", overwrite = T)

plot(map.RLMN.p3)




##C50 DECISION TREES
fm
library(C50)
set.seed(8400)
training <- sample(nrow(dat_subset), 0.7 * nrow(dat_subset))

c50.model <- C5.0(x = dat_subset[training, c("ANALYTICAL_HILLSHADING", "CONVERGENCE_INDEX", "VALLEY_DEPTH",
                                             "NDVI_SIBUNDOY_CLIP","CROSS_SEC_CURVATURE")], y = dat_subset$H2O2_1[training], trials = 1,
                  rules = F, control = C5.0Control(CF = 0.95, minCases = 20,
                                                   earlyStopping = FALSE))

c50.model
summary(c50.model)

plot(c50.model)

predict(c50.model, newdata = dat_subset[training, ])

map.C5.c <- predict(COV84, c50.model, type = "class",
                    filename = "C50_5m_PRED__1_CAJ_06092018.tif",
                    format = "GTiff", overwrite = T)
plot(map.C5.c)







ls(getModelInfo())

#PARA PERFILES
#======RANDOM FOREST=====
########IMPORTANCIA DE LAS COVARIABLES

dat_subset <- read.csv("E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\rmat.csv",sep=";")
TAX <- readOGR(dsn="E:\\SIBUNDOY\\SIBUNDOY_covariables\\COV_5M\\PERFILES_VF.shp")
head(TAX)
names(TAX)
dat_subset <- join(x=dat_subset,y=TAX@data,by="PERFIL")
dim(dat_subset)
names(dat_subset)
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
summary(dat_subset)
getwd()
##VARIABLE IMPORTANCE 

cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)
control2 <- rfeControl(functions=nbFuncs, method="repeatedcv", number=5, repeats=5)

(RFE_RF_model <- rfe(dat_subset[-c(1:4,23:31)], factor(dat_subset[,28]), sizes=c(1:6), rfeControl=control2) )
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




