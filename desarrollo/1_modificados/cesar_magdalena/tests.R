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

#oad
readRDS('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/covariables.rds')