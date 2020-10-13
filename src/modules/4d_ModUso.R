#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModUso <- function(VarObj){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg = c('caret','raster')

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
  modelos.datos.entrada <- paste0(proyecto.directorio,'/modelos/0_particion/',str_replace(VarObj,'[.]','-'))
  datos.entrada <- paste0(proyecto.directorio,'/datos/salida/1_covariables')
  datos.salida <- paste0(proyecto.directorio,'/prediccion')
  dir.create(datos.salida, recursive = T, mode = "0777", showWarnings = F)
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/1_modelos/',str_replace(VarObj,'[.]','-'))
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/2_analisis/tabular/',str_replace(VarObj,'[.]','-'))

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particición
  load(paste0(modelos.datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))

  # Cargar 1_covariables
  COV <- stack(paste0(datos.entrada,'/raster/covariables.tif'))
  names(COV) <- readRDS(paste0(datos.entrada,'/rds/nombrescovariables.rds'))

  #identificar mejor modelo
  modelos.resultado <- read.csv(file = paste0(modelos.analisis.tabular,'/mejoresmodelos_parametros.csv'))

  if (is(train.data[,'target'],'numeric')){
    modelos.mejor <- modelos.resultado[modelos.resultado$RMSE == min(modelos.resultado$RMSE), 'modelo']
  } else if (is(train.data[,'target'],'factor')){
    modelos.mejor <- modelos.resultado[modelos.resultado$Accuracy == max(modelos.resultado$Accuracy), 'modelo']
  }

  ##### output messages ####
  cat(paste0('### RESULTADO 1 de 2: El mejor modelo es ',modelos.mejor,' y se comprueba si ya existe archivo GeoTIFF de prediccion ###','\n'))
  ##### end output messages ####

  get(load(paste0(modelos.entrada,'/',modelos.mejor,".rds",sep="")))

  prediccion.archivo <- paste0(datos.salida,'/',str_replace(VarObj,'[.]','-'),'_PRED_',modelos.mejor,'.tif')
  if (!file.exists(prediccion.archivo)){
    ##### output messages ####
    cat(paste0('### RESULTADO 2 de 2: El archivo GeoTIFF de predicción usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo,' ###','\n'))
    ##### end output messages ####
    start <- Sys.time()
    PredictGeoTIFF(COV, modelo.ajuste, prediccion.archivo)
    print(Sys.time() - start)
  } else{
    ##### output messages ####
    cat(paste0('### RESULTADO 2 de 2: El archivo GeoTIFF de predicción usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', prediccion.archivo,' ###','\n'))
    ##### end output messages ####
  }

}