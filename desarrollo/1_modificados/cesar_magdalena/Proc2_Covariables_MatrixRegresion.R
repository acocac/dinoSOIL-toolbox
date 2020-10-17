##=====================================================================================
#Codigo necesario para generar la matriz de regresion para el mapeo de pH con las
#distinteas opciones de calculo de valores a los intervalos de profundidad deseados
#0-30 cm
#30-100 cm
#======================================================================================
rm(list=ls())

setwd("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\CODIGOS")

pckg = c('readxl','raster','sp',
         'rgdal','magrittr','gdalUtilities',
          'rgeos','sf','spdplyr','smoothr')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

lapply(pckg,usePackage)

#-----------------------------------
#Carga de limite de zona de estudio y verificar que es solo un único poligono
#-----------------------------------
lim <- readOGR('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite')
if (dim(lim)[1] > 1){
  lim$id <- 0
  lim <- aggregate(lim, 'id')
}
plot(lim)
#----------------------
#Carga de 1_covariables
#----------------------

#Derivados del DEM
cov <- stack("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\INSUMOS\\DEM_DERIVADOS.tif")
ls <- c("DEM","AnHill","Slo","Aspect","Plan_Curv",
                "Prof_Curv","Conv_Ind","Closs_Dep",
                "TotCatchAr","TWI","LSfactor","CHBL",
                "CND","VD","RSP","MRVBF","MRRTF","TRI",
                "visSky","SkyVF","Pos_Op","Neg_Op","TPI")

cov <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/dem/derivados/TIF/DERIVADOS.tif')
plot(cov)

dem <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/dem/original/DEM_PT_2020.tif')
dem_derivado <- stack(list.files(path='/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/dem/derivados/individuales', pattern='tif', all.files=FALSE, full.names=TRUE,recursive=TRUE))
cov <- stack(dem, dem_derivado)
names(cov)[1] <- 'DEM'

cov <- crop(cov,lim)
cov <- mask(cov,lim)
plot(cov[[1]])
plot(lim,add=T)

require(dplyr)
#Clima
clima_files <- list.files(path = '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/clima', pattern = "\\.shp$", full.names=TRUE)
clima_list <- lapply(clima_files, readOGR)
clima_list_target <- lapply(clima_list, "[", c('Denominaci'))
clima <- raster::intersect(clima_combined, lim)
clima@data[['Denominaci']] = gsub(",", "", clima@data[['Denominaci']])
clima <- aggregate(clima, 'Denominaci')
clima <- fill_holes(clima, units::set_units(1, km^2))
clima <- spTransform (clima, CRS=projection(cov))
clima$denom_char <- as.factor(clima$Denominaci)
clima_rast <- gRasterize(clima, cov, "denom_char")
clima_rast_res <- resample(clima_rast,cov,method="ngb")
values(clima_rast_res) <- round(values(clima_rast_res),0)
names(clima_rast_res) <- 'clima'
cov <- stack(cov,clima_rast_res)
names(cov)
rm(clima_rast)


#Geomorfologia- tipo de relieve
tr <- readOGR('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/geomorfologia')
tr <- readOGR("/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/geomorfologia/GEOMORFOLOGIA_CESAR_MAGDALENA_V7_20200309.shp")
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

##Union de valores de pH a base de variables taxon�micas y coordenadas
data <- plyr::join(data,data1,by="profileId")


data$EPIPEDON <- as.factor(data$EPIPEDON)
data$ENDOPEDON <- as.factor(data$ENDOPEDON)
data$ORDEN <- as.factor(data$ORDEN)
data$SUBORDEN <- as.factor(data$SUBORDEN)
data$GRANGRUPO <- as.factor(data$GRANGRUPO)
data$FAMILIA_TE <- as.factor(data$FAMILIA_TE)

#### Juntamos 1_covariables con los datos  ####

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
