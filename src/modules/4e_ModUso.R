#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################


prompt.user.part4e <- function()#get arguments from user
{
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  variables.usuario <- VariablesObjectivo()
  cat(paste0('Las siguientes columnas estan disponibles para su modelación:','\n'))
  cat(paste0(variables.usuario, sep=" | "))
  cat(paste0('\n','\n'))

  message(prompt="Indique el nombre de la variable objetivo de acuerdo al listado superior:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el tipo de base de datos para modelar (AMBAS, PERFIL, OBSERVACION):>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  message(prompt="Indique el numero limite de covariables a considerar según interpretación del RFE y Boruta:>>> ")
  c <- readLines(n = 1)
  c <- gsub("\\\\", "/", c)

  message(prompt="Si la variable es categorica indique la estrategia usada para balancear los datos (UP, DOWN), caso contrario que prefiera desbalanceado o la variable es continua escriba ORIGINAL:>>> ")
  d <- readLines(n = 1)
  d <- gsub("\\\\", "/", d)

  message(prompt="Indique si usar los algoritmos por DEFECTO o del archivo CONFIG:>>> ")
  e <- readLines(n = 1)
  e <- gsub("\\\\", "/", e)

  newlist = list(a, b, c, d, e)

  return(newlist)
}


ModUso <- function(VarObj, BaseDatos, rfe_lim, Muestreo, listmodelos){
  # iniciar el monitoreo tiempo de procesamiento total
  start_time <- Sys.time()

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  #pckg = c('caret','raster', 'sf', 'stringr', 'doParallel', 'ggspatial')
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(caret, raster, sf, stringr, doParallel, ggspatial, pals, gridExtra, viridis, purrr))

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/5_Predict.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[['proyecto.carpeta']]
  proyecto.metricas.categoricas <- conf.args[['metricas.categoricas']]
  proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))
  proyecto.metricas.continuas <- conf.args[['metricas.continuas']]
  proyecto.metricas.continuas = unlist(strsplit(proyecto.metricas.continuas,';'))
  
  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  modelos.datos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
  datos.entrada <- paste0(proyecto.directorio,'/datos/salida/1_covariables')
  datos.salida.geotiff <- paste0(proyecto.directorio,'/prediccion/',BaseDatos,'/geotiff/', str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo, '/', listmodelos)
  dir.create(datos.salida.geotiff, recursive = T, mode = "0777", showWarnings = F)
  datos.salida.figuras <- paste0(proyecto.directorio,'/prediccion/',BaseDatos,'/figuras/', str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo, '/', listmodelos)
  dir.create(datos.salida.figuras, recursive = T, mode = "0777", showWarnings = F)
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/2_modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo, '/', listmodelos)
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/3_analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo, '/', listmodelos)
  modelos.incertidumbre.raster = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/4_incertidumbre/geotiff/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo, '/', listmodelos)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particición
  print(modelos.datos.entrada)
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
  modelos.resultado <- read.csv(file = paste0(modelos.analisis.tabular,'/mejoresmodelos_metricas.csv'))
  
  if (is(train.data[,'target'],'numeric')){
    metrica <- paste0(proyecto.metricas.continuas[1],'.Median')
    modelos.mejor <- modelos.resultado[modelos.resultado[,metrica] == min(modelos.resultado[,metrica]), 'modelos']
    index <- 1
    type <- 'raw'
  } else if (is(train.data[,'target'],'factor')){
    metrica <- paste0(proyecto.metricas.categoricas[1],'.Median')
    modelos.mejor <- modelos.resultado[modelos.resultado[,metrica] == max(modelos.resultado[,metrica]), 'modelos']
    index <- 1:nlevels(train.data[['target']])
    type <- 'prob'
  }
  
  ##### output messages ####
  cat(paste0('### RESULTADO 1 de 3: El mejor modelo es ',modelos.mejor,' y se comprueba si ya existe archivo GeoTIFF de prediccion ###','\n'))
  ##### end output messages ####

  m <- get(load(paste0(modelos.entrada,'/',modelos.mejor,".rds",sep="")))

  prediccion.archivo.geotiff <- paste0(datos.salida.geotiff,'/',str_replace(VarObj,'[.]','-'),'_PRED_',modelos.mejor,'.tif')
  if (!file.exists(prediccion.archivo.geotiff)){
    ##### output messages ####
    cat(paste0('### RESULTADO 2 de 3: El archivo GeoTIFF de predicción usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo.geotiff,' ###','\n'))
    ##### end output messages ####
    if (is(train.data[,'target'],'numeric')){
      start <- Sys.time()
      PredictGeoTIFF(COV, modelo.ajuste, prediccion.archivo.geotiff, type, index, train.data)
      print(Sys.time() - start)
    } else if (is(train.data[,'target'],'factor')){
      proba.archivo.geotiff <- paste0(modelos.incertidumbre.raster,'/',str_replace(VarObj,'[.]','-'),'_PROB_',modelos.mejor,'.tif')
      prediction_prob <- stack(proba.archivo.geotiff)

      start <- Sys.time()
      no_cores <- detectCores() - 1
      beginCluster(no_cores)
      clusterR(prediction_prob, fun = calc, args = list(fun = nnet::which.is.max),
                            filename = prediccion.archivo.geotiff, format = "GTiff",
                            overwrite = T, datatype='INT1U', options='COMPRESS=YES')
      endCluster()
      print(Sys.time() - start)

    }

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

    if (is(train.data[,'target'],'numeric')){
      p <- ggplot() +
      annotation_map_tile(zoomin = -1) +
      layer_spatial(pred, aes(fill = stat(band1)), alpha = 0.7) +
      scale_fill_viridis_c(name=str_replace(VarObj,'[.]','-'),na.value = NA, direction=-1, option="viridis", alpha = 0.7) +
      annotation_scale(location = "tl") +
      annotation_north_arrow(location = "br", which_north = "true")
    } else if (is(train.data[,'target'],'factor')){

      pred <- ratify(pred)
      rat <- levels(pred)[[1]]
      rat$class <- levels(train.data[['target']])[rat$ID]
      levels(pred) <- rat

      pred2 <- deratify(pred, 'class')

      n<-length(levels(pred2)[[1]]$class)

      cols <- pals::cols25(n)

      p <- rasterVis::levelplot(pred2, maxpixels = ncell(pred2), col.regions = cols, par.settings = list(axis.line = list(col = 'transparent'),
          strip.background = list(col = 'transparent'),
          strip.border = list(col = 'transparent')),
          scales = list(col = 'transparent'))

      #p <- gplot(my_rst$category) +
      #annotation_map_tile(zoomin = -1) +
      #geom_raster(aes(fill=factor(value),interpolate=FALSE), alpha=0.7) +
      #scale_fill_manual(na.translate=TRUE, na.value = "black", values = cols,name= "Clases") +
      #annotation_scale(location = "tl") +
      #annotation_north_arrow(location = "br", which_north = "true")
    }
    png(file = prediccion.archivo.figuras, width = 1400, height = 900, res=150, bg = "transparent")
    print(p)
    dev.off()
  } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 3 de 3: La figura de predicción de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', prediccion.archivo.figuras,' ###','\n'))
    ##### end output messages ####
  }

  #estimar tiempo de procesamiento total
  print(Sys.time() - start_time)
}