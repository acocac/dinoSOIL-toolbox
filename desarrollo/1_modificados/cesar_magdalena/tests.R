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

figuras.dir <- '/proyecto_cesarmagdalena/prediccion/AMBAS/figuras'
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

load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/AMBAS/0_particion/pH-0_30_spline/particion.RData')
train.data <- as.data.frame(particion['train'])
dim(train.data)
names(train.data) <- sub("test.", "", names(train.data))
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

# Mejorar plots RFE
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/rds/pH-0_30_spline/boruta.rds')
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/rds/pH-0_30_spline/rfe.rds')

png('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/figuras/pH-0_30_spline/rfe.png', width = 700, height = 550)
plot(rfmodel, type=c('g', 'o'), cex=2, metric = "RMSE")
dev.off()

png('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/figuras/pH-0_30_spline/boruta.png', width = 700, height = 550)
par(mar = c(18, 4, 1, 1))
plot(bor, cex.axis=1.3, las=2, xlab="", cex=0.75)
dev.off()

plot(bor, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(bor$ImpHistory),function(i)
        bor$ImpHistory[is.finite(bor$ImpHistory[,i]),i])
      names(lz) <- colnames(bor$ImpHistory)
      Labels <- sort(sapply(lz,median))
      axis(side = 1,las=2,labels = names(Labels),
           at = 1:ncol(bor$ImpHistory), cex.axis = 0.7)
     #Now, we'll plot the boruta variable importance chart.

plot(bor, xlab = "")
#plot function in Boruta adds the attribute values to the x-axis horizontally where all the attribute values are not dispayed due to lack of space.
lz <-lapply(seq_len(ncol(bor$ImpHistory)), function(i)
  bor$ImpHistory[is.finite(bor$ImpHistory[,i]),i])
names(lz) <- colnames(bor$ImpHistory)
axis(side = 1, cex.axis = 0.2)

v <- varImp(rfmodel$fit, type = 1, scale = F)
v[,"covar"] <- row.names(v)
v <- v[order(v$Overall,decreasing = T),]
top_species <- v[1:8,"covar"]

#plot top 25 geo indicator species fig s13
png('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/figuras/test.png')
par(font = 3)
dotchart(rev(v[1:8,"Overall"])*100,labels= rev(top_species),cex=1.2,pt.cex = 1.3,
         xlab="Mean decrease in accuracy", mgp = c(2.2,0,0))
dev.off()

importances <- sort(rfmodel$fit$importance[,1],decreasing = TRUE)
barplot(importances, cex.names=0.55, cex.axis=0.5)

### probar mapas
require(maps)
map("world", "Colombia")
map.cities(country = "Colombia", capitals = 2)

require(ows4R)
require(httr)
wfs_regions <- "https://eservices.minfin.fgov.be/arcgis/services/R2C/Regions/MapServer/WFSServer"
url <- parse_url(wfs_regions)
url$query <- list(service = "wfs",
                  version = "2.0.0",
                  request = "GetFeature",
                  typename = "regions",
                  srsName = "EPSG:4326",
                  outputFormat = "GEOJSON")
request <- build_url(url)

bel_regions <- read_sf(request) #Lambert2008
ggplot(bel_regions) +
  geom_sf()

### cortar geometrias
require(sf)
require(dplyr)
vias <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/data/igac/500k/via_t1_limite.shp')
limite <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/ZONAS_VIDA_CESAR_MARGDALENA.shp')
if (dim(limite)[1] > 1){
    limite$id <- 0
    limite <- limite %>% group_by(id) %>% summarize()
}

dem <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/dem/original/DEM_PT_2020.tif')

vias2 < vias  %>%
  st_transform(CRS(dem))
limite2 <- limite %>%
  st_transform(CRS(dem))

vias <- st_as_sf(vias)

intersect_test <- st_intersection(vias,limite)

st_is_valid(vias)

plot(intersect_test)
dev.off()

##localizacion entrenamiento
# Cargar particición
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/0_particion/pH-0_30_spline/particion.RData')
train.data <- as.data.frame(particion['train'])
names(train.data) <- sub("train.", "", names(train.data))
names(train.data)

##terra
install.packages("terra")
require(terra)
require(sf)

cov <- terra::rast('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.tif')
names(cov) <- readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.rds')
get(load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/1_modelos/pH-0_30_spline/8_covariables/RandomForest.rds'))

limite <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba')
if (dim(limite)[1] > 1){
    limite$id <- 0
    limite <- limite %>% group_by(id) %>% summarize()
}

cov_subset <- terra::crop(cov, limite)

start <- Sys.time()
a <- terra::predict(cov_subset, modelo.ajuste)
print(Sys.time() - start)

start <- Sys.time()
b <- raster::predict(cov_subset, modelo.ajuste)
print(Sys.time() - start)

require(doParallel)
require(raster)
start <- Sys.time()
no_cores <- detectCores() - 1
raster::beginCluster(no_cores)
raster::clusterR(cov_subset, predict, args = list(modelo.ajuste),
      filename = out.file, format = "GTiff",
      overwrite = T)
raster::endCluster()
print(Sys.time() - start)

##familia textural
tex <- read.csv('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/0_matriz/MatrixDatos.csv')
names(tex)

library(tidyr)
tex2 <- tex %>% drop_na(names(tex[16:41]))

tex2 <-tex[na.omit(tex[17:41]),]

tex2 <- na.omit(tex, cols = names(tex[17:41]))

tex2 <- tex %>% drop_na(clima)

any(is.na(tex2$MRVBF))

require(readxl)
write.csv(table(tex2$FAMILIA_TE), paste0('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/enviado/familiaT.csv'), row.names = F)
write

##convert rdata
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/0_particion/pH-30_100_spline/particion.RData')

train = particion['train']
test = particion['test']

write.csv(train, '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/enviado/arosso/pH-30_100_spline/train.csv', row.names = F)
write.csv(test, '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/enviado/arosso/pH-30_100_spline/test.csv', row.names = F)

##load rde
require(caret)
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/AMBAS/rds/pH-30_100_spline/rfe.rds')
rfe_lim = 8
covars = predictors(rfmodel)[c(1:rfe_lim)]
paste(covars, collapse=', ')

z = c('train_correlationmatrix','test_correlationmatrix', a)
e = 'train_correlationmatrix'
sprintf('%s_clima', a)

lista.graficos.dispersion = c(sprintf('%s_dispersion_clima', covars), sprintf('%s_dispersion_clima', covars),sprintf('%s_dispersion_clima', covars))
gsub('dispersion','boxplot', lista.graficos.dispersion)
difs <- setdiff(z,e)


## crop area
require(terra)
require(sf)
cov <- terra::rast('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.tif')
names(cov) <- readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.rds')
get(load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/1_modelos/pH-0_30_spline/8_covariables/RandomForest.rds'))

paste(names(cov), collapse=', ')
length(names(cov))

limite <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba')
if (dim(limite)[1] > 1){
    limite$id <- 0
    limite <- limite %>% group_by(id) %>% summarize()
}

train.data <- as.data.frame(particion['train'])
names(train.data) <- sub("train.", "", names(train.data))
colfinal <- names(train.data)[2:ncol(train.data)]


cov_subset <- terra::crop(cov, limite)

cov_subset2 <- raster::subset(cov_subset, colfinal, value = T)

writeRaster(cov_subset2, '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/enviado/arosso/area2.tif', drivers = 'GTiff', overwrite=TRUE, NAflag=0, options='COMPRESS=LZW')

## ver capa
clima_raster = raster::raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/raster/clima/clima.tif')
clima_vector <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba')
if (dim(limite)[1] > 1){
    limite$id <- 0
    limite <- limite %>% group_by(id) %>% summarize()
}


## best model parameters
modelos.resultado <- read.csv(file = '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/3_analisis/tabular/pH-0_30_spline/8_covariables/mejoresmodelos_parametros.csv')
modelos.mejor <- modelos.resultado[modelos.resultado$RMSE == min(modelos.resultado$RMSE), 'modelo']

get(load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/2_modelos/pH-0_30_spline/8_covariables/RandomForest.rds'))

modelo.ajuste$bestTune$mtry


##plot raster
r <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/AMBAS/4_incertidumbre/geotiff/pH-0_30_spline/8_covariables/incertidumbre_residuales_media.tif')

library(ggplot2)
library(ggspatial)
load_longlake_data()

ggplot() +
  # loads background map tiles from a tile source
  annotation_map_tile(zoomin = -1) +

  # annotation_spatial() layers don't train the scales, so data stays central
  annotation_spatial(longlake_roadsdf, size = 2, col = "black") +
  annotation_spatial(longlake_roadsdf, size = 1.6, col = "white") +

  # raster layers train scales and get projected automatically
  layer_spatial(longlake_depth_raster, aes(colour = stat(band1))) +
  # make no data values transparent
  scale_fill_viridis_c(na.value = NA, direction=-1, option="inferno") +

  # layer_spatial trains the scales
  layer_spatial(longlake_depthdf, aes(fill = DEPTH_M)) +

  # spatial-aware automagic scale bar
  annotation_scale(location = "tl") +

  # spatial-aware automagic north arrow
  annotation_north_arrow(location = "br", which_north = "true")

##GRANGRUPO cantidad grupos
matriz <- read.csv('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/0_matriz/MatrixDatos.csv')
names(matriz)
table(matriz$GRANGRUPO)

matriz2 <- matriz[matriz$GRANGRUPO %in%  names(table(matriz$GRANGRUPO))[table(matriz$GRANGRUPO) >= 5] , ]

##GRANGRUPO graficos RFE
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/AMBAS/rds/GRANGRUPO/rfe.rds')
rfe_lim<-7
predictors(rfmodel)[c(1:rfe_lim)]
paste0(predictors(rfmodel)[c(1:rfe_lim)], collapse=", ")

png('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/figuras/pH-0_30_spline/rfe.png', width = 700, height = 550)
plot(rfmodel, type=c('g', 'o'), cex=2, metric = "Accuracy")
dev.off()
rfmodel
png('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/figuras/pH-0_30_spline/boruta.png', width = 700, height = 550)
par(mar = c(18, 4, 1, 1))
plot(bor, cex.axis=1.3, las=2, xlab="", cex=0.75)
dev.off()

##check partition groups
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/AMBAS/0_particion/GRANGRUPO/particion.RData')

train = as.data.frame(particion['train'])
test = as.data.frame(particion['test'])
names(train) <- sub("train.", "", names(train))
names(test) <- sub("test.", "", names(test))

is(train[,'target'],'factor')

table(train$target)
table(test$target)

names(table(train$target))
names(table(test$target))

down_train <- downSample(x = train[, -ncol(train)],
                         y = train$target)
down_train$Class <- NULL

smote_train <- SMOTE(target ~ ., data  = train)

table(down_train$target)

##GRANGRUPO evaluacion
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/AMBAS/0_particion/GRANGRUPO/particion.RData')

sampling_strategy = 'ORIGINAL'
load(paste0('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/AMBAS/2_modelos/GRANGRUPO/7_covariables/',sampling_strategy,'/RandomForest.rds'))

test = as.data.frame(particion['test'])
names(test) <- sub("test.", "", names(test))
write.csv(test, '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/enviado/patricia/grangrupo/v1/evaluacion.csv', row.names = F)

train = as.data.frame(particion['train'])
names(train) <- sub("train.", "", names(train))
write.csv(train, '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/enviado/patricia/grangrupo/v1/entrenamiento.csv', row.names = F)

pred <- predict(modelo.ajuste, test)
pred_prob <- predict(modelo.ajuste, test, type = "prob")
dim(pred_prob)
caret::confusionMatrix(pred,test$target)$overall["Accuracy"]
caret::confusionMatrix(pred,test$target)$overall["Kappa"]

## GRANGRUPO grafico
devtools::install_github("thomasp85/scico")
library(scico)
#https://github.com/thomasp85/scico
library(RColorBrewer)
library(rasterVis)
pred <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion/AMBAS/geotiff/GRANGRUPO/7_covariables/ORIGINAL/GRANGRUPO_PRED_RandomForest.tif')
levels(pred)
pred$category
r <- ratify(pred)
rat <- levels(r)[[1]]

rat$class <- levels(train[['target']])[rat$ID]

levels(pred)
library(pals)

pred
rat <- levels(pred)[[1]]
rat$class <- levels(train[['target']])[rat$ID]
levels(Predictions) <- rat

my_rst <- deratify(pred, 'category')

n<-length(levels(pred2)[[1]]$category)
names <- levels(pred2)[[1]]$category
if (levels(pred2)[[1]]$category[1] == ''){
  n <- n -1
  names <- names[-1]
}
cols <- pals::cols25(n)
names(cols) <- names

rasterVis::levelplot(pred2, col.regions = cols)

pred2
require(sf)
limite_shp <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba')

p <- ggplot() +
    annotation_map_tile(zoomin = -1) +
    layer_spatial(pred, aes(fill = stat(band1)), alpha = 0.7) +
    scale_fill_discrete() +
    annotation_scale(location = "tl") +
    annotation_north_arrow(location = "br", which_north = "true")
print(p)



my_palette <- brewer.pal(n = length(levels(my_rst)[[1]]$category), name = "Dark2")
my_palette <- c(my_palette,'red')
levels(my_rst)
as.character(levels(my_rst)[[1]]$category)
names(my_palette) <- length(levels(my_rst)[[1]]$category)
my_palette

n<-length(levels(my_rst)[[1]]$category)
cols <- pals::cols25(n)
names(cols) <- levels(my_rst)[[1]]$category

levels(my_rst)
names(cols)[-1]
p <- gplot(my_rst$category) +
    annotation_map_tile(zoomin = -1) +
    geom_raster(aes(fill=factor(value),interpolate=FALSE)) +
    scale_fill_manual(na.translate=FALSE, values = cols,name= "My name", labels=names(cols)[-1])
    #scale_fill_discrete("LC Class")
    #layer_spatial(my_rst, aes(fill = factor(stat(band1))), alpha = 0.7) +
    #scale_fill_manual(name='grangrupo',palette="nuuk",na.value="transparent") +
print(p)

p <- gplot(pred) +
    annotation_map_tile(zoomin = -1) +
    geom_raster(aes(fill=factor(value),interpolate=FALSE), alpha=0.7) +
    scale_fill_discrete(na.translate=TRUE, na.value = "black", name= "Clases") +
    annotation_scale(location = "tl") +
    annotation_north_arrow(location = "br", which_north = "true")
print(p)

levels(pred)
rat <- read.csv('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion/AMBAS/geotiff/GRANGRUPO/7_covariables/ORIGINAL/GRANGRUPO_PRED_RandomForest.csv')
n<-length(rat$class)
cols <- pals::cols25(n)
names(cols) <- rat$class
pred2 <- deratify(pred,'category',overwrite=TRUE)
pred2

levels(pred2)
unique(pred2)
table(pred2)
cellStats(pred2, 'sum')

factorValues(pred2, pred2[c(3, 12)])

p <- gplot(pred2) +
    annotation_map_tile(zoomin = -1) +
    geom_raster(aes(fill=factor(value),interpolate=FALSE), alpha=0.7) +
    scale_fill_discrete(na.translate=TRUE, na.value = "black", name= "Clases") +
    annotation_scale(location = "tl") +
    annotation_north_arrow(location = "br", which_north = "true")
print(p)

n<-length(levels(pred2)[[1]]$category)
cols <- pals::cols25(n)
names(cols) <- levels(pred2)[[1]]$category

p <- gplot(pred2) +
    annotation_map_tile(zoomin = -1) +
    geom_raster(aes(fill=factor(value),interpolate=FALSE), alpha=0.7) +
    scale_fill_manual(na.translate=FALSE, values = cols,name= "My name", labels=names(cols)[-1]) +
    annotation_scale(location = "tl") +
    annotation_north_arrow(location = "br", which_north = "true")
print(p)

p <- gplot(pred2) +
    annotation_map_tile(zoomin = -1) +
    geom_raster(aes(fill=factor(value),interpolate=FALSE), alpha=0.7) +
    scale_fill_manual(na.translate=FALSE, values = cols,name= "My name") +
    annotation_scale(location = "tl") +
    annotation_north_arrow(location = "br", which_north = "true")
print(p)

##capas incertidumbre
inc <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion/AMBAS/geotiff/GRANGRUPO/7_covariables/ORIGINAL/GRANGRUPO_INCERTIDUMBRE_RandomForest.tif')

colr = viridis(100, direction=-1, begin = 0, end = 1)
names(inc) <- c('Indice de Shannon', 'Indice de Confusión')
plot(inc, zlim=c(0, 1), col=colr, axes=FALSE, box=FALSE)


plot(inc[[2]])

maxValue(inc)
minValue(inc)

rat <- read.csv('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion/AMBAS/geotiff/GRANGRUPO/7_covariables/ORIGINAL/GRANGRUPO_PRED_RandomForest.csv')
length(rat$class)
prob <- stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion/AMBAS/geotiff/GRANGRUPO/7_covariables/ORIGINAL/GRANGRUPO_PROB_RandomForest.tif')
dim(prob)
plot(prob[1])

plot(prob[[1]])

nlevels(train[['target']])
unique()
unique(names(table(train$target)))

rNA <- sum(is.na(prob))

target_df <- as.data.frame(prob[[1]], xy = TRUE)

cols <- pals::coolwarm()
colr = inferno(100, direction=-1, begin = 0, end = 1)

dev.new(height=0.91*nrow(prob)/50, width=1.09*ncol(prob)/50)
plot(prob, zlim=c(0, 1), col=colr, axes=FALSE, box=FALSE, legend.args = list(text = 'Probabilidad', side = 4, font = 2, line = 2.5, cex = 0.5))
plot(prob, zlim=c(-10,10), legend.only=T)

legend("top", inset=.02, title="Probability",
   c("0","1"), fill=cols, horiz=TRUE, cex=0.8)

names(prob) <- levels(train$target)
prob_plots <- list()
for (i in 1:length(names(prob))){
  target <- prob[[i]]
  target_df <- as.data.frame(target, xy = TRUE)
  p <- ggplot() +
  geom_raster(data = target_df,
              aes(x = x, y = y,
                  fill = as.numeric(names(prob)[i]))) +
  scale_fill_viridis_c(name='Probabilidad',na.value = NA, direction=-1, option="plasma", limits = c(0, 1)) +
  theme_void() +
  theme(legend.position = "none") +
  ggtitle(names(prob)[i])

  prob_plots[[i]] <- p

  #p <- ggplot() +
  #annotation_map_tile(zoomin = -1, alpha=0.7) +
  #layer_spatial(prob[[i]], alpha = 0.7) +
  #scale_fill_viridis_c(name='Probabilidad',na.value = NA, direction=-1, option="plasma", limits = c(0, 1), alpha = 0.7) +
  #theme_void() +
  #theme(legend.position = "none") +
  #ggtitle(names(prob)[i])
  #prob_plots[[i]] <- p
}

library(ggpubr)
n <- length(prob_plots)
nCol <- floor(sqrt(n))
p2 <- do.call("ggarrange", c(prob_plots, ncol=nCol, common.legend = TRUE, legend="bottom"))
annotate_figure(p2, top = text_grob("Main title", face = "bold", size = 16))

p2 <- ggarrange(plotlist=prob_plots, ncol=nCol, common.legend = TRUE, legend="bottom")
annotate_figure(p2, top = text_grob("Probabilidades por clase", face = "bold", size = 16))

## verificar prediccion categoricas
load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/AMBAS/0_particion/GRANGRUPO/particion.RData')

train.data = as.data.frame(particion['train'])
names(train.data) <- sub("train.", "", names(train))

pred <- raster('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/prediccion/AMBAS/geotiff/GRANGRUPO/7_covariables/ORIGINAL/GRANGRUPO_PRED_RandomForest.tif')
maxpixels = ncell(pred)
pred <- ratify(pred)
rat <- levels(pred)[[1]]
rat$class <- levels(train.data[['target']])[rat$ID]
levels(pred) <- rat