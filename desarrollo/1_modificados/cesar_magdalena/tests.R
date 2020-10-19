## procesamiento en paralelo ## TODO
fn = system.file('/proyecto_cesarmagdalena/datos/salida/1_covariables/raster/covariables.TIF', package = "rgdal")
obj <- rgdal::GDALinfo(fn)
fn <- stack(fn)

## mapas de las predicciones ##
require(raster)
require(stringr)
#Viz raster
prediction.file<- 'pH-30_100_spline_PRED_RandomForest.tif'
prediction.dir <- '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion'
pred <- raster(paste0(prediction.dir,'/',prediction.file))

figuras.dir <- '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion/figuras'
png(file = paste0(figuras.dir,'/',str_replace(prediction.file,'.tif','.png')), width = 700, height = 600)
plot(pred, main = 'pH (0-30cm)')
dev.off()

## mapas de las predicciones ##
csc.list <- mapply(c, rep("CSC", 16), c(1:16), SIMPLIFY=FALSE)
paste0(csc.list,collapse = '-')

## pH
pH <- read.csv('/proyecto_cesarmagdalena/datos/salida/0_basededatos/BD_pH_Spl_SumPond_mio.csv', sep=',', na='NA')

## Matriz regresion
matriz <- read.csv('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/tabular/MatrixRegresion.csv', sep=';', na='NA')

write.table(matriz, '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/tabular/MatrixRegresion_correcta.csv', row.names = F, sep=',')


##
#### Criterios de busqueda ####
shp <- st_read('/Volumes/Alejo/Users/ac/Documents/temp.shp')
shp.df <- data.frame(shp)

relieve <- list()
for (i in sort(unique(shp.df$tipo_relie))){
  freq <- table(shp.df[shp.df$tipo_relie == i, 'TIPO_REL_5'])
  relieve.tmp <- names(freq[which(freq == max(freq))])
  print(relieve.tmp)
  relieve <- append(relieve,relieve.tmp)
}
level <- sort(unique(shp.df$tipo_relie))
final <- paste0(level,' (',relieve,') ', collapse = ';')

##list
a_rest <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/separados/relieve/relieve.tif')
b_rest <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/separados/relieve/relieve.tif')
c_rest <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/separados/relieve/relieve.tif')

d<-stack(a_rest,b_rest,c_rest)
str(C45.Models)
list_obj = objects(pattern="*_rest")

output <- lapply(list_obj, function(x) get(x))

a<-stack(output)
dim(a)

##sobreponer puntos poligono
dem <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/separados/dem/original/DEM_PT_2020.tif')
points <- data.frame(read_excel('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/0_basededatos/originales/BD_OBSERVACIONES_MODELAMIENTO_PT_2020_09092020v1.xlsx', sheet = 'Hoja1', na = "N/A"))
coordinates(points) <- ~ LONGITUD + LATITUD
proyeccion <- CRS("+proj=longlat +datum=WGS84")
points@proj4string <- proyeccion
points <- spTransform(points, CRS=projection(dem))
points <- st_make_valid(points)
points<-st_as_sf(points, coords=c("LONGITUD","LATITUD"))
st_crs(points) <- proyeccion
points2<- st_transform(points, projection(poligono))

poligono <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/ZONAS_VIDA_CESAR_MARGDALENA.shp')
if (dim(poligono)[1] > 1){
    poligono$id <- 0
    #poligono <- aggregate(poligono, 'id')
    poligono <- poligono %>% group_by(id) %>% summarize()
}
poligono
plot(poligono)
poligono<- st_transform(poligono, projection(poligono))

out <- st_intersection(points2, poligono)
plot(out)

#load
b<-readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.rds')
a<-stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.tif')
b

#comparar
modelo <- get(load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/1_modelos/pH-0_30_spline/RandomForest.rds'))

#covariables geotiff
require(raster)
require(sf)
limite_shp <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba')

load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/0_particion/pH-0_30_spline/particion.RData')
train.data <- as.data.frame(particion['train'])
names(train.data) <- sub("train.", "", names(train.data))
vars_modelos <- names(train.data)[which(names(train.data) != 'target')]
z <- names(cov)[which(!names(cov) %in% vars_modelos)]

cov4 <- dropLayer(cov, z)
dim(cov4)
cov <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.tif')
names(cov) <- readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.rds')
#cov[is.na(cov)] <- 0
cov <- crop(cov4,limite_shp)
cov <- mask(cov,limite_shp)
cov[is.na(cov)] <- 0
cov3 <- cov
cov$ClosedDepressions <- 0
dim(cov)
writeRaster(cov,'/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables_final.tif', drivers = 'GTiff', overwrite=TRUE, NAflag=0)
dim(cov)
names(cov)
max(cov$ClosedDepressions)
hist(cov,
     maxpixels=ncell(cov), na.rm=TRUE)
hist(cov$ClosedDepressions,maxpixels=ncell(cov$ClosedDepressions))
cov_na<- cov
for (i in names(cov)){
  print(i)
  #print(any(is.na(values(cov[[i]]))))
  f <- freq(cov[[i]], value=NA) / ncell(cov[[i]])
  print(f)
}
f <- freq(cov, value=NA) / ncell(cov)
i <- which(f <= 0.5)
i
names(cov)
cov2 <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/old/covariables.tif')
names(cov2) <- readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/old/covariables.rds')
cov2 <- crop(cov2,limite_shp)
dim(cov2)
for (i in names(cov2)){
  print(i)
  #print(any(is.na(values(cov2[[i]]))))
  f <- freq(cov2[[i]], value=NA) / ncell(cov2[[i]])
  print(f)
}

hist(cov2$Closs_Dep,
     maxpixels=ncell(cov2))
max(values(cov2$Closs_Dep))
## predict error
modelo <- '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/1_modelos/pH-0_30_spline/RandomForest.rds'
get(load(modelo))
require(waldo)
waldo::compare(cov2$ChanneNetworkBaseLevel,cov$ChanneNetworkBaseLevel)
cov3 <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.tif')

target <- data.frame(ChanneNetworkBaseLevel=87.2,dem=90,ValleyDepth=468,clima=3)
s2 <- predict(cov2, modelo.ajuste)
plot(s2)

names(cov2)[which(names(cov2) == 'CHBL')] <- 'ChanneNetworkBaseLevel'
names(cov2)[which(names(cov2) == 'DEM')] <- 'dem'
names(cov2)[which(names(cov2) == 'VD')] <- 'ValleyDepth'

predict(modelo.ajuste, cov2[1,1])
predict(modelo.ajuste, cov[1,1])

dim(cov)
#predict
f <- list(levels(as.factor(unique(values(cov$clima)))),levels(as.factor(unique(values(cov$tipo_relieve)))))
names(f) <- c('clima','tipo_relieve')
s1 <- raster::predict(cov, modelo.ajuste)
plot(s1)
s2 <- predict(cov2, modelo.ajuste)
plot(s2)
names(cov)
waldo::compare(s1,s2)

names(cov[1:20,1:5,1:26])