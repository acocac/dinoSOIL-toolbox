#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModMejorModelo <- function(VarObj, rfe_lim){
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
  proyecto.modelos.categoricas <- conf.args[[5]]
  proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))
  proyecto.modelos.continuas <- conf.args[[6]]
  proyecto.modelos.continuas = unlist(strsplit(proyecto.modelos.continuas,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  datos.entrada <- paste0(proyecto.directorio,'/modelos/0_particion/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/2_modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/3_analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(modelos.analisis.tabular, recursive = T, mode = "0777", showWarnings = F)
  modelos.analisis.figuras = paste0(proyecto.directorio,'/modelos/3_analisis/figuras/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(modelos.analisis.figuras, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particición
  load(paste0(datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))

  #identificar modelos entrenados
  modelos.procesados = strsplit(list.files(modelos.entrada, recursive = T, full.names = F),"[.]")
  modelos.procesados = c(unique(sapply(modelos.procesados, "[", 1)))

  if (sort(proyecto.modelos.continuas) == sort(modelos.procesados)){
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
    cat(paste('### RESULTADO 1 de 3: The best models parameters and perfomance results were generated and store as tabular data in the model (tabular) folder! ###'),'\n')
    ##### end output messages ####
    C45.Models = objects(pattern="*C45")
    C50.Models = objects(pattern="*C50")
    RF.Models = objects(pattern="*RandomForest")
    xgbTree.Models = objects(pattern="*xgbTree")
    SVM.Models = objects(pattern="*svmRadial")
    MLP.Models = objects(pattern="*mlp")
    DM.Models = c(C45.Models,C50.Models,RF.Models,SVM.Models,xgbTree.Models,MLP.Models)

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
  cat(paste('### RESULTADO 2 de 3: The best models statistics of perfomance results were generated and store as tabular data in the model (tabular) folder! ###','\n'))
  ##### end output messages ####

  if (is(train.data[,'target'],'numeric')){
    #boxplots charts
    png(file = paste0(modelos.analisis.figuras,'/boxplots_modelos.png'), width = 700, height = 600)
    print(bwplot(resamps, metric=c('RMSE','Rsquared'), layout = c(2, 1), box.ratio = 1, auto.key = T, scales=list(tck=c(1,0), x=list(cex=1.5), y=list(cex=1.5))))
    dev.off()
  } else if (is(train.data[,'target'],'factor')){
    png(file = paste0(modelos.analisis.figuras,'/boxplots_modelos.png'), width = 700, height = 600)
    print(bwplot(resamps, metric=c('Accuracy','OA'), layout = c(2, 1), box.ratio = 1, auto.key = T))
    dev.off()
  }

  ##### output messages ####
  cat(paste('### RESULTADO 3 de 3: The boxplots to compare the best models according to perfomance variables were generated and store as chart in the models (figures) folder! ###'),'\n')
  ##### end output messages ####

  } else{
    cat('Los modelos listados en el archivo config.txt NO estan completos')
  }
}