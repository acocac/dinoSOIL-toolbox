#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModMejorModelo <- function(){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg = c('data.table','PerformanceAnalytics','GGally',
           'caret','nnet','plyr')

  usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  
  lapply(pckg,usePackage)

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[[1]]
  modelos.proyecto <- conf.args[[2]]
  modelos.proyecto = unlist(strsplit(modelos.proyecto,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
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
  #identificar modelos entrenados
  modelos.procesados = strsplit(list.files(modelos.entrada, recursive = T, full.names = F),"[.]")
  modelos.procesados = c(unique(sapply(modelos.procesados, "[", 1)))
  if (modelos.proyecto == modelos.procesados){
    cat('Los modelos listados en el archivo config.txt estan completos')
    ##### processing ####
    #merge models results
    modelos.parametros = NULL
    for (modelo in modelos.procesados){
      get(load(paste0(modelos.entrada,'/',modelo,".rds",sep="")))
      bestTuneIndex <- as.numeric(rownames(modelo.ajuste$bestTune)[1])
      modelo.mejor <- modelo.ajuste$results[bestTuneIndex, 1:dim(modelo.ajuste$results)[2]]
      modelo.mejor = cbind('modelo'=modelo,modelo.mejor)
      modelos.parametros = rbind.fill(modelos.parametros, modelo.mejor)
      assign(modelo, modelo.ajuste)
    }
    #export models best combination parameters and features
    write.csv(modelos.parametros,file = paste0(modelos.analisis.tabular,'/mejoresmodelos_parametros.csv'), row.names=T)
    ##### output messages ####
    cat(paste('### RESULT 1 out of 4: The best models parameters and perfomance results were generated and store as tabular data in the model (tabular) folder! ###'),'\n')
    ##### end output messages ####
    C45.Models = objects(pattern="*C45")
    RF.Models = objects(pattern="*RandomForest")
    DM.Models = c(C45.Models,RF.Models)

    #create list for resampling
    resampling.list <- list()
    for(DM.Model in DM.Models){
      a <- get(DM.Model)
      name <- DM.Model
      resampling.list[[name]] <- a
    }

  #resampling for model comparison
  resamps <- resamples(resampling.list)

  #comparison statistics
  modelos.comparacion = data.frame(summary(resamps)$statistics)
  #export models comparison (boxplot)
  write.csv(modelos.comparacion,paste0(modelos.analisis.tabular,"/bestmodelsCV_boxplots.csv",sep=""), row.names=T)

  ##### output messages ####
  cat(paste('### RESULT 2 out of 4: The best models statistics of perfomance results were generated and store as tabular data in the model (tabular) folder! ###','\n'))
  ##### end output messages ####

  #boxplots charts
  png(file = paste0(modelos.analisis.figuras,'/boxplots_modelos.png'), width = 700, height = 600)
  print(bwplot(resamps, layout = c(2, 1), box.ratio = 1, auto.key = T))
  dev.off()

  ##### output messages ####
  cat(paste('### RESULT 3 out of 3: The boxplots to compare the best models according to perfomance variables were generated and store as chart in the models (figures) folder! ###'),'\n')
  ##### end output messages ####

  } else{
    cat('Los modelos listados en el archivo config.txt NO estan completos')
  }
}