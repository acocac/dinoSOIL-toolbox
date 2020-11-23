#############################################################################
# titulo        : Mejor modelo;
# proposito     : Identificar mejor modelo de acuerdo a la metrica seleccionada;
# autor(es)     : Preparado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado por ACC en Bogota, Colombia en Septiembre 2020;;
# entrada       : Archivos de modelos en formato rds;
# salida        : Tablas y graficas indicando metricas de cada modelo;
# observaciones : ninguna;
##############################################################################

ModMejorModelo <- function(VarObj, BaseDatos, rfe_lim, Muestreo, listmodelos){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  #pckg = c('data.table','PerformanceAnalytics','GGally',
  #         'caret','nnet','plyr')
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(data.table, PerformanceAnalytics, caret, plyr, dplyr, purrr, stringr))

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/3b_modelsettings.R'))
  
  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[[1]]
  
  if (listmodelos == 'DEFECTO'){
    configuracion <- modelos.config.defecto()
    proyecto.modelos.continuas <- configuracion[[1]]
    proyecto.modelos.categoricas <- configuracion[[2]]
    
    proyecto.metricas.categoricas <- conf.args[[8]]
    proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))
    proyecto.metricas.continuas <- conf.args[[9]]
    proyecto.metricas.continuas = unlist(strsplit(proyecto.metricas.continuas,';'))
    
  } else{
    proyecto.modelos.categoricas <- conf.args[[6]]
    proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))
    proyecto.modelos.continuas <- conf.args[[7]]
    proyecto.modelos.continuas = unlist(strsplit(proyecto.modelos.continuas,';'))
    
    proyecto.metricas.categoricas <- conf.args[[8]]
    proyecto.metricas.categoricas = unlist(strsplit(proyecto.metricas.categoricas,';'))
    proyecto.metricas.continuas <- conf.args[[9]]
    proyecto.metricas.continuas = unlist(strsplit(proyecto.metricas.continuas,';'))
  }

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  datos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
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
  # Cargar particiciÃ³n
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
    
    #pamatros
    modelos.parametros <- modelos.parametros[,!grepl(wild, names(modelos.parametros), perl = TRUE)]
    
    #export models best combination parameters and features
    write.csv(modelos.parametros,file = paste0(modelos.analisis.tabular,'/mejoresmodelos_parametros.csv'), row.names=F)
    
    ##### output messages ####
    cat(paste('### RESULTADO 1 de 3: Los parametros y métricas de los mejores modelos fueron generados y almacenados como tablas en la ruta ', modelos.analisis.tabular,' ###'),'\n','\n')
    ##### end output messages ####
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
  
  ##### output messages ####
  cat(paste('### RESULTADO 2 de 3: Los valores para realizar boxplots de los modelos creados fueron generados y almacenados como archivo tabular en la ruta ', modelos.analisis.tabular,' ###'),'\n','\n')
  ##### end output messages ####

  scales <- list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5, labels=rev(modelos.metricas$modelos)))
  #scales <- list(x=list(relation="free"), y=list(relation="free"), tck=c(1,0), x=list(cex=2), y=list(cex=2))
  
  png(file = paste0(modelos.analisis.figuras,'/boxplots_modelos.png'), width = 700, height = 600)
  print(bwplot(resamps, scales=scales, metric=proyecto.metricas, layout = c(2, 1), box.ratio = 1, auto.key = T))
  dev.off()

  ##### output messages ####
  cat(paste('### RESULTADO 3 de 3: Los gráficos boxplots de los modelos creados fueron generados y almacenados como archivo PNG en la ruta', modelos.analisis.figuras,' ###'),'\n','\n')
  ##### end output messages ####

  } else{
    cat('Los modelos listados en el archivo config.txt NO estan completos, se recomienda revisar el/los modelo(s) faltante corriendo el componente 4b Entrenamiento del modelo')
  }
}