###############################################################################
# # Covariable.R
# # R Version: R version >= 4.0.2
# #
# # Author(s): Victoria Camacho & Yesenia Vargas
# #
# # Description: NDVI con GEE
# # 
# # Inputs: shp del �rea de estudio
# #	
# #
# # Outputs: 
# #
# #
# # File history:
# #   06102020: Creacion
# #   10102020: Modificacion
# #   
###############################################################################
rm(list=ls())
# #
# # Paths
inPath  <- "/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/temp"
outPath <- "./output/"
src     <- getwd()

remotes::install_github("r-spatial/rgee")

# # Load libraries
pckg <- c('reticulate', 'remotes', 'rgee', 'mapview',
          'sf', 'geojsonio', 'googledrive')

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

lapply(pckg,usePackage)

## It is necessary just once
ee_install()
# Initialize Earth Engine!
#ee_Initialize(drive = TRUE)
ee_Initialize(email = 'acocac@gmail.com')

pathshp <- file.path('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba/limite_prueba_sirgas.shp')

maskL8sr <- function(image){
  cloudShadowBitMask = bitwShiftL(1, 3);
  cloudsBitMask = bitwShiftL(1, 5);
  qa = image$select('pixel_qa');
  mask = qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
  return(image$updateMask(mask))
}

#### Criterios de busqueda ####
limite.shp <- st_read(pathshp)
if (dim(limite.shp)[1] > 1){
  limite.shp$id <- 0
  limite.shp <- aggregate(limite.shp, 'id')
}
limite.shp@crs
b <- st_transform(b, 32618)

shp <- st_read(pathshp)%>%
  sf_as_ee()
collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")
start <- ee$Date("2019-01-01")
finish <- ee$Date("2020-08-30")

#### Busqueda en la coleccion L8 #### 
filteredCollection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$
  filterBounds(shp)$
  filterDate(start, finish)$
  filterMetadata('CLOUD_COVER', 'less_than',30)$
  map(maskL8sr)

#### Mediana de la coleccion ####
median <- filteredCollection$reduce(ee$Reducer$median())
vizParams <- list(
  bands = c("B5_median", "B4_median", "B3_median"),
  min = 5000,
  max = 15000,
  gamma = 1.3
)
Map$addLayer(median, vizParams, "Landsat 8 medianas")
#### NDVI #### 
getNDVI <- function(image) {
  return(image$normalizedDifference(c("B5_median", "B4_median")))
}
ndvi <- getNDVI(median)
clipndvi <- ndvi$clip(shp)

ndviParams <- list(palette = c(
  "blue", "white", "green"
))

resolution = filteredCollection$first()$select('B5')$projection()$nominalScale()$getInfo()

#### Visualizacion ####
Map$centerObject(shp)
Map$addLayer(clipndvi, ndviParams, "NDVI")

#### Exportar drive ####
task_img <- ee_image_to_drive(
  image = clipndvi,
  fileFormat = "GEO_TIFF",
  fileNamePrefix = "my_image_demo",
  folder = "rgee_backup", 
  region = ee_ROI
)

task_img <- ee_image_to_drive(
  image = ndvi,
  fileFormat = "GEO_TIFF",
  fileNamePrefix = "my_image_demo",
  folder = "rgee_backup",
  region = shp$geometry(),
  scale = resolution
)

task_img$start()
ee_monitoring(task_img)

imgloc <- ee_drive_to_local(task = task_img)

require(raster)
raster <- raster(imgloc)
plot(raster)