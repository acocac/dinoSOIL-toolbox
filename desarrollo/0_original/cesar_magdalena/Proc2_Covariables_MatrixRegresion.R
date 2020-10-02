##=====================================================================================
#Codigo necesario para generar la matriz de regresion para el mapeo de pH con las
#distinteas opciones de calculo de valores a los intervalos de profundidad deseados
#0-30 cm
#30-100 cm
#======================================================================================
rm(list=ls())

setwd("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\CODIGOS")
library(readxl)
library(raster)
library(sp)
library(rgdal)
library(magrittr)
library(gdalUtilities)
library(rgeos)

#-----------------------------------
#Carga de limite de zona de estudio
#-----------------------------------

lim <- readOGR("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\LIMITE\\LIMTE_PT2020.shp")

#----------------------
#Carga de covariables
#----------------------

#Derivados del DEM
cov <- stack("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\DEM_DERIVADOS.tif")
names(cov) <- c("DEM","AnHill","Slo","Aspect","Plan_Curv",
                "Prof_Curv","Conv_Ind","Closs_Dep",
                "TotCatchAr","TWI","LSfactor","CHBL",
                "CND","VD","RSP","MRVBF","MRRTF","TRI",
                "visSky","SkyVF","Pos_Op","Neg_Op","TPI")
cov <- crop(cov,lim)
cov <- mask(cov,lim)
plot(cov[[1]])
plot(lim,add=T)

#Clima
clima <- readOGR("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\COVARIABLES\\Clima\\CLIMA_PT_2020_vf.shp")
clima <- spTransform (clima, CRS=projection(cov))
clima$denom_char <- as.factor(clima$DENOMINACI)
clima_rast <- gRasterize(clima, cov, "denom_char")
clima_rast_res <- resample(clima_rast,cov,method="ngb")
values(clima_rast_res) <- round(values(clima_rast_res),0)
names(clima_rast_res) <- 'clima'
# (cl_dummy <- dummyRaster(MATPAR_rast_res))
# names(mp_dummy) <- levels(factor(MATPAR$MP_RAST))
# cov <- stack(cov,MATPAR_rast_res, mp_dummy)
cov <- stack(cov,clima_rast_res)
names(cov)
rm(clima_rast)


#Geomorfologia- tipo de relieve
tr <- readOGR("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\COVARIABLES\\Geomorfologia\\Geomorfologia_03092020.shp")
tr <- spTransform (tr, CRS=projection(cov))
tr$TIPO_RELIE <- as.character(tr$TIPO_RELIE)
tr_rast <- gRasterize(tr, cov, "TIPO_RELIE")
tr_rast_res <- resample(tr_rast,cov,method="ngb")
values(tr_rast_res) <- round(values(tr_rast_res),0)
names(tr_rast_res) <- 'tipo_relieve'
# (cl_dummy <- dummyRaster(MATPAR_rast_res))
# names(mp_dummy) <- levels(factor(MATPAR$MP_RAST))
# cov <- stack(cov,MATPAR_rast_res, mp_dummy)
cov <- stack(cov,tr_rast_res)
names(cov)


#NDVI
ndvi <- raster("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\COVARIABLES\\Ces_Mag_NDVI.tif")
ndvi <- projectRaster(ndvi,cov,method="bilinear")
names(ndvi) <- "ndvi"
cov <- stack(cov,ndvi)
names(cov)


#----------------------
#Mariz de regresion
#----------------------

##Carga de base con variables a modelar
data <- read_excel("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\BASES\\BD_09092020_modelos.xlsx") %>% data.frame

##Carga de base pH
data1 <- read.csv("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\BASES\\BD_pH_Spl_SumPond.csv")

##Union de valores de pH a base de variables taxonómicas y coordenadas
data <- plyr::join(data,data1,by="profileId")


data$EPIPEDON <- as.factor(data$EPIPEDON)
data$ENDOPEDON <- as.factor(data$ENDOPEDON)
data$ORDEN <- as.factor(data$ORDEN)
data$SUBORDEN <- as.factor(data$SUBORDEN)
data$GRANGRUPO <- as.factor(data$GRANGRUPO)
data$FAMILIA_TE <- as.factor(data$FAMILIA_TE)

#### Juntamos covariables con los datos  ####

dat_subset_sp <- data
coordinates(dat_subset_sp) <- ~ LONGITUD + LATITUD
proy <- CRS("+proj=longlat +datum=WGS84")
dat_subset_sp@proj4string <- proy
dat_subset_sp <- spTransform(dat_subset_sp, CRS=projection(cov))


start <- Sys.time()
#dat <- extract(cov, dat_subset_sp, sp = TRUE)
data <- cbind(data, raster::extract(cov, dat_subset_sp))
summary(data)
print(Sys.time() - start)

spplot(cov[[26]])

write.csv(data,"E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\BASES\\MatrixRegresion_29092020.csv",row.names = F)

writeRaster(cov,"E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\COVARIABLES\\Covariables_PT_2020.tif")
saveRDS(names(cov),"E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\COVARIABLES\\NombresCovariables_PT_2020.rds")
