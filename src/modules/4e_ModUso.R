#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModUso <- function(VarObj, BaseDatos, rfe_lim){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg = c('caret','raster', 'sf', 'stringr', 'doParallel', 'ggspatial')

  usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  
  lapply(pckg,usePackage)

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/5_Predict.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[[1]]
  modelos.proyecto <- conf.args[[2]]
  modelos.proyecto <- sort(unlist(strsplit(modelos.proyecto,';')))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  modelos.datos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
  datos.entrada <- paste0(proyecto.directorio,'/datos/salida/1_covariables')
  datos.salida.geotiff <- paste0(proyecto.directorio,'/prediccion/',BaseDatos,'/geotiff/', str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(datos.salida.geotiff, recursive = T, mode = "0777", showWarnings = F)
  datos.salida.figuras <- paste0(proyecto.directorio,'/prediccion/',BaseDatos,'/figuras/', str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(datos.salida.figuras, recursive = T, mode = "0777", showWarnings = F)
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/2_modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/3_analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particición
  load(paste0(modelos.datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))
  train.variables <- names(train.data)[which(names(train.data) != 'target')]

  # Cargar 1_covariables y eliminar capas no usadas en el entrenamiento
  COV <- stack(paste0(datos.entrada,'/covariables.tif'))
  names(COV) <- readRDS(paste0(datos.entrada,'/covariables.rds'))
  capas.eliminar <- names(COV)[which(!names(COV) %in% train.variables)]
  COV <- dropLayer(COV, capas.eliminar)

  #identificar mejor modelo
  modelos.resultado <- read.csv(file = paste0(modelos.analisis.tabular,'/mejoresmodelos_parametros.csv'))

  if (is(train.data[,'target'],'numeric')){
    modelos.mejor <- modelos.resultado[modelos.resultado$RMSE == min(modelos.resultado$RMSE), 'modelo']
  } else if (is(train.data[,'target'],'factor')){
    modelos.mejor <- modelos.resultado[modelos.resultado$Accuracy == max(modelos.resultado$Accuracy), 'modelo']
  }

  ##### output messages ####
  cat(paste0('### RESULTADO 1 de 3: El mejor modelo es ',modelos.mejor,' y se comprueba si ya existe archivo GeoTIFF de prediccion ###','\n'))
  ##### end output messages ####

  get(load(paste0(modelos.entrada,'/',modelos.mejor,".rds",sep="")))

  prediccion.archivo.geotiff <- paste0(datos.salida.geotiff,'/',str_replace(VarObj,'[.]','-'),'_PRED_',modelos.mejor,'.tif')
  if (!file.exists(prediccion.archivo.geotiff)){
    ##### output messages ####
    cat(paste0('### RESULTADO 2 de 3: El archivo GeoTIFF de predicción usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo.geotiff,' ###','\n'))
    ##### end output messages ####
    start <- Sys.time()
    PredictGeoTIFF(COV, modelo.ajuste, prediccion.archivo.geotiff)
    print(Sys.time() - start)
  } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 2 de 3: El archivo GeoTIFF de predicción usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', prediccion.archivo.geotiff,' ###','\n'))
    ##### end output messages ####
  }

  prediccion.archivo.figuras <- paste0(datos.salida.figuras,'/',str_replace(VarObj,'[.]','-'),'_PRED_',modelos.mejor,'.png')
  if (!file.exists(prediccion.archivo.figuras)){
    ##### output messages ####
    cat(paste0('### RESULTADO 3 de 3: La figura de predicción de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo.figuras,' ###','\n'))
    ##### end output messages ####
    pred <- raster(prediccion.archivo.geotiff)

    p <- ggplot() +
    annotation_map_tile(zoomin = -1) +
    layer_spatial(pred, aes(fill = stat(band1)), alpha = 0.7) +
    scale_fill_viridis_c(name=str_replace(VarObj,'[.]','-'),na.value = NA, direction=-1, option="viridis", alpha = 0.7) +
    annotation_scale(location = "tl") +
    annotation_north_arrow(location = "br", which_north = "true")

    png(file = prediccion.archivo.figuras, width = 700, height = 600)
    print(p)
    #print(plot(pred, main = VarObj))
    dev.off()
  } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 3 de 3: La figura de predicción de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', prediccion.archivo.figuras,' ###','\n'))
    ##### end output messages ####
  }

}