#############################################################################
# titulo        : Mejor modelo;
# proposito     : Identificar mejor modelo de acuerdo a la metrica seleccionada;
# autor(es)     : Preparado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# creacion      : Creado por ACC en Bogota, Colombia en Septiembre 2020; Actualizado por ACC en Diciembre 2020;
# entrada       : Archivos de los modelos en formato rds;
# salida        : Tablas y graficas para identificar el mejor modelo;
# observaciones : ninguna;
##############################################################################


prompt.user.part4c <- function()#get arguments from user
{
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  variables.usuario <- VariablesObjectivo()
  cat(paste0('Las siguientes columnas estan disponibles para su modelacion:','\n'))
  cat(paste0(variables.usuario, sep=" | "))
  cat(paste0('\n','\n'))

  message(prompt="Indique el nombre de la variable objetivo de acuerdo al listado superior:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el tipo de base de datos para modelar (AMBAS, PERFIL, OBSERVACION):>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  message(prompt="Indique el numero limite de covariables a considerar segun interpretacion del RFE y Boruta:>>> ")
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


ModMejorModelo <- function(VarObj, BaseDatos, rfe_lim, Muestreo, listmodelos){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  if (!require('pacman')) install.packages('pacman');

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(data.table, PerformanceAnalytics, caret, plyr, dplyr, purrr, stringr))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/4_ConfigModelos.R'))
  
  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Remover espacio en blanco de la variable
  VarObj <- trimws(VarObj)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[['proyecto.carpeta']]

  if (listmodelos == 'DEFECTO'){
    configuracion <- modelos.config.defecto()
    proyecto.modelos.continuas <- configuracion[['modelos.continuas']]
    proyecto.modelos.categoricas <- configuracion[['modelos.categoricas']]

    proyecto.metricas.categoricas <- conf.args[['metricas.categoricas']]
    proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))
    proyecto.metricas.continuas <- conf.args[['metricas.continuas']]
    proyecto.metricas.continuas = unlist(strsplit(proyecto.metricas.continuas,';'))
    
  } else{
    proyecto.modelos.categoricas <- conf.args[['modelos.categoricas']]
    proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))
    proyecto.modelos.continuas <- conf.args[['modelos.continuas']]
    proyecto.modelos.continuas = unlist(strsplit(proyecto.modelos.continuas,';'))

    proyecto.metricas.categoricas <- conf.args[['metricas.categoricas']]
    proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))
    proyecto.metricas.continuas <- conf.args[['metricas.continuas']]
    proyecto.metricas.continuas = unlist(strsplit(proyecto.metricas.continuas,';'))
  }

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  datos.entrada <- trimws(paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-')))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/2_modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables','/',toupper(Muestreo), '/', listmodelos)
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/3_analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/',toupper(Muestreo), '/', listmodelos)
  dir.create(modelos.analisis.tabular, recursive = T, mode = "0777", showWarnings = F)
  modelos.analisis.figuras = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/3_analisis/figuras/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/',toupper(Muestreo), '/', listmodelos)
  dir.create(modelos.analisis.figuras, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))
  
  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particion
  load(paste0(datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))

  if (is(train.data[,'target'],'numeric')){
    proyecto.modelos <- proyecto.modelos.continuas
    proyecto.metricas <- proyecto.metricas.continuas
  }
  else if (is(train.data[,'target'],'factor')){
    proyecto.modelos <- proyecto.modelos.categoricas
    proyecto.metricas <- proyecto.metricas.categoricas
  }

  #identificar modelos entrenados
  modelos.procesados = strsplit(list.files(modelos.entrada, recursive = T, full.names = F),"*.rds")
  modelos.procesados = unique(sapply(modelos.procesados, "[", 1))

  if (sort(proyecto.modelos) == sort(modelos.procesados)){
    cat(paste0('Los modelos listados en el archivo config.txt estan completos','\n','\n'))
    ##### processing ####
    #merge models results
    modelos.parametros = NULL
    for (modelo in modelos.procesados){
      get(load(paste0(modelos.entrada,'/',modelo,".rds")))
      bestTuneIndex <- as.numeric(rownames(modelo.ajuste$bestTune)[1])
      modelo.mejor <- modelo.ajuste$results[bestTuneIndex, 1:dim(modelo.ajuste$results)[2]]
      modelo.mejor = cbind('modelo'=modelo,modelo.mejor)
      modelos.parametros = rbind.fill(modelos.parametros, modelo.mejor)
      assign(paste0('m_',modelo), modelo.ajuste)
    }
    
    #metricas
    wild <- paste0(modelo.ajuste$perfNames,collapse='|')
    
    #parametros
    modelos.parametros <- modelos.parametros[,!grepl(wild, names(modelos.parametros), perl = TRUE)]
    
    #export models best combination parameters and features
    write.csv(modelos.parametros,file = paste0(modelos.analisis.tabular,'/mejoresmodelos_parametros.csv'), row.names=F)
    
    ##### mensaje de salida ####
    cat(paste('### RESULTADO 1 de 3: Los parametros y metricas de los mejores modelos fueron generados y almacenados como tablas en la ruta ', modelos.analisis.tabular,' ###'),'\n','\n')
    ##### final del mensaje de salida ####
    models = objects(pattern='*m_')

    #create list for resampling
    resampling.list <- list()
    for(m in models){
      a <- get(m)
      name <- m
      resampling.list[[name]] <- a
    }

  #resampling for model comparison
  resamps <- resamples(resampling.list)

  #remover prefijo
  models = gsub('m_','',models)
  
  #metricas modelos
  modelos.metricas = data.frame('modelos'=models,summary(resamps)$statistics)
  modelos.metricas <- modelos.metricas %>% arrange_at(paste0(proyecto.metricas[1],'.Median'), desc)
  #exportar metricas modelos
  write.csv(modelos.metricas,paste0(modelos.analisis.tabular,"/mejoresmodelos_metricas.csv",sep=""), row.names=F)
  
  ##### mensaje de salida ####
  cat(paste('### RESULTADO 2 de 3: Los valores para realizar boxplots de los modelos creados fueron generados y almacenados como archivo tabular en la ruta ', modelos.analisis.tabular,' ###'),'\n','\n')
  ##### final del mensaje de salida ####

  scales <- list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5, labels=rev(modelos.metricas$modelos)))

  png(file = paste0(modelos.analisis.figuras,'/boxplots_modelos.png'), width = 700, height = 600)
  print(bwplot(resamps, scales=scales, metric=proyecto.metricas, layout = c(2, 1), box.ratio = 1, auto.key = T))
  dev.off()

  ##### mensaje de salida ####
  cat(paste('### RESULTADO 3 de 3: Los graficos boxplots de los modelos creados fueron generados y almacenados como archivo PNG en la ruta', modelos.analisis.figuras,' ###'),'\n','\n')
  ##### final del mensaje de salida ####

  } else{
    cat('Los modelos listados en el archivo config.txt NO estan completos, se recomienda revisar el/los modelo(s) faltante corriendo el componente 4b Entrenamiento del modelo')
  }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))
}