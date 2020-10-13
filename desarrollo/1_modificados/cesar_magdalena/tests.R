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


