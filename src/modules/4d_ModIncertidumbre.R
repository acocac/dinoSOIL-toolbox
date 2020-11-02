#############################################################################
# titulo        : Incertidumbre del modelo;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModIncertidumbre <- function(VarObj, rfe_lim){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg = c('caret','raster', 'sf', 'stringr', 'doParallel',
           'Metrics', 'lime', 'quantregForest')

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
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  modelos.datos.entrada <- paste0(proyecto.directorio,'/modelos/0_particion/',str_replace(VarObj,'[.]','-'))
  datos.entrada <- paste0(proyecto.directorio,'/datos/salida/1_covariables')
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/2_modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/3_analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  modelos.incertidumbre.figuras = paste0(proyecto.directorio,'/modelos/4_incertidumbre/figuras/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(modelos.incertidumbre.figuras, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar partición
  load(paste0(modelos.datos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))
  train.variables <- names(train.data)[which(names(train.data) != 'target')]
  test.data <- as.data.frame(particion['test'])
  names(test.data) <- sub('test.', "", names(test.data))

  # Cargar RFE
  load(paste0(exploratorio.variables,'/rfe.rds'))

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

  #vimp <- data.frame(variables=predictors(modelo.ajuste),
  #                 importance=as.vector(modelo.ajuste$variable.importance))


  pred <- predict(modelo.ajuste, test.data)
  (AVE <- 1 - sum((pred-test.data$target)^2, na.rm=TRUE)/
      sum((test.data$target - mean(test.data$target, na.rm = TRUE))^2,
          na.rm = TRUE))
  print(AVE)
  print(Metrics::rmse(pred,test.data$target))
  print(cor(pred,test.data$target))

  pairplot <- ggplot(data.frame("target"=test.data$target, "pred"=pred), aes(target, pred)) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)

  png(file = paste0(modelos.incertidumbre.figuras,'/pairplot.png'), width = 700, height = 600)
  print(pairplot)
  dev.off()

  residuals <- ggplot(data.frame("target"=test.data$target, "residual"=test.data$target - pred),
       aes(target, residual)) +
  geom_point() +
  geom_abline(slope=0, intercept=0) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)

  png(file = paste0(modelos.incertidumbre.figuras,'/residuals.png'), width = 700, height = 600)
  print(residuals)
  dev.off()
  #
  #td.sort <- train.data[order(train.data$target),]
  #td.sort <- td.sort[,predictors(rfmodel)[c(1:rfe_lim)]]
  #explainer <- lime(td.sort, modelo.ajuste)
  #print(names(td.sort))
  #print(explainer)
  #
  ## Explaining selecting samples
  #explanation_ini <- explain(td.sort[1:5,],
  #                           explainer,
  #                           n_permutations = 5000,
  #                           dist_fun = "euclidean",
  #                           n_features = 5,
  #                           feature_select = "highest_weights")

  #explanation_fin <- explain(td.sort[317:321,],
  #                           explainer,
  #                           n_permutations = 5000,
  #                           dist_fun = "euclidean",
  #                           n_features = 5,
  #                           feature_select = "highest_weights")
  #
  ## Visualising the model explanations
  #plot_features(explanation_ini)
  #plot_features(explanation_fin)
  #plot_explanations(explanation_ini)
  #plot_explanations(explanation_fin)

  # Mapa de incertidumbre #
  no_cores <- detectCores() - 1
  beginCluster(no_cores)
  model <- quantregForest(y=test.data$target - pred, x=test.data[,predictors(rfmodel)[1:rfe_lim]], ntree=500, keep.inbag=TRUE)
  #Estimate model uncertainty
  unc <- clusterR(COV[[predictors(rfmodel)[1:rfe_lim]]], predict, args=list(model=model,what=sd), filename = '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/temp.dir/unc.tif')
  endCluster()
  #salvar el mapa de incertidumbre
  plot(unc)

  #prediccion.archivo.geotiff <- paste0(datos.salida.geotiff,'/',str_replace(VarObj,'[.]','-'),'_PRED_',modelos.mejor,'.tif')
  #if (!file.exists(prediccion.archivo.geotiff)){
  #  ##### output messages ####
  #  cat(paste0('### RESULTADO 2 de 3: El archivo GeoTIFF de predicción usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo.geotiff,' ###','\n'))
  #  ##### end output messages ####
  #  start <- Sys.time()
  #  PredictGeoTIFF(COV, modelo.ajuste, prediccion.archivo.geotiff)
  #  print(Sys.time() - start)
  #} else{
  #  ##### output messages ####
  #  cat(paste0('### RESULTADO 2 de 3: El archivo GeoTIFF de predicción usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', prediccion.archivo.geotiff,' ###','\n'))
  #  ##### end output messages ####
  #}
  #
  #prediccion.archivo.figuras <- paste0(datos.salida.figuras,'/',str_replace(VarObj,'[.]','-'),'_PRED_',modelos.mejor,'.png')
  #if (!file.exists(prediccion.archivo.figuras)){
  #  ##### output messages ####
  #  cat(paste0('### RESULTADO 3 de 3: La figura de predicción de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', prediccion.archivo.figuras,' ###','\n'))
  #  ##### end output messages ####
  #  pred <- raster(prediccion.archivo.geotiff)
  #  plot(pred)
  #  png(file = prediccion.archivo.figuras, width = 700, height = 600)
  #  print(plot(pred, main = VarObj))
  #  dev.off()
  #} else{
  #  ##### output messages ####
  #  cat(paste0('### RESULTADO 3 de 3: La figura de predicción de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', prediccion.archivo.figuras,' ###','\n'))
  #  ##### end output messages ####
  #}

}