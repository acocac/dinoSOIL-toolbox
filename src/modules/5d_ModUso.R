#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModUso <- function(){
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
  datos.entrada <- paste0(proyecto.directorio,'/datos/entrada/1_covariables')
  datos.salida <- paste0(proyecto.directorio,'/datos/salida/1_predicciones')
  dir.create(datos.salida, recursive = T, mode = "0777", showWarnings = F)
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/rds')
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/tabular')
  dir.create(modelos.analisis.tabular, recursive = T, mode = "0777", showWarnings = F)
  modelos.analisis.figuras = paste0(proyecto.directorio,'/modelos/figuras')
  dir.create(modelos.analisis.figuras, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar covariables
  COV <- stack(paste0(datos.entrada,'/color/raster/COV_VF.tif'))
  names(COV) <- readRDS(paste0(datos.entrada,'/color/rds/NAMES_COV_VF_COLOR.rds'))

  #identificar mejor modelo
  modelos.resultado <- read.csv(file = paste0(modelos.analisis.tabular,'/mejoresmodelos_parametros.csv'))
  modelos.mejor <- modelos.resultado[modelos.resultado$Accuracy == max(modelos.resultado$Accuracy), 'modelo']
  ##### output messages ####
  cat(paste0('### RESULTADO 1 de 1: El mejor modelo es ',modelos.mejor,' y es usado para prediccion ###','\n'))
  ##### end output messages ####

  get(load(paste0(modelos.entrada,'/',modelos.mejor,".rds",sep="")))

  PredictGeoTIFF(COV, modelo.ajuste, datos.salida, modelos.mejor)

}