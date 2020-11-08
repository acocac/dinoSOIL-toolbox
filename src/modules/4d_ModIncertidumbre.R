#############################################################################
# titulo        : Incertidumbre del modelo;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModIncertidumbre <- function(VarObj, BaseDatos, rfe_lim, Muestreo){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(caret, raster, sf, stringr, doParallel,
                                  Metrics, lime, quantregForest, hydroGOF,
                                  RColorBrewer, rasterVis, classInt, ggspatial, viridis,
                                  sf, plyr, dplyr))

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
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/',BaseDatos,'/rds/',str_replace(VarObj,'[.]','-'))
  modelos.datos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
  datos.entrada <- paste0(proyecto.directorio,'/datos/salida/1_covariables')
  in.geo.data <- paste0(proyecto.directorio,'/datos/entrada/1_covariables')
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/2_modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo)
  modelos.analisis.tabular = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/3_analisis/tabular/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo)
  modelos.incertidumbre.figuras = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/4_incertidumbre/figuras/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo)
  dir.create(modelos.incertidumbre.figuras, recursive = T, mode = "0777", showWarnings = F)
  modelos.incertidumbre.raster = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/4_incertidumbre/geotiff/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo)
  dir.create(modelos.incertidumbre.raster, recursive = T, mode = "0777", showWarnings = F)
  modelos.incertidumbre.modelo = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/4_incertidumbre/modelo/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables/', Muestreo)
  dir.create(modelos.incertidumbre.modelo, recursive = T, mode = "0777", showWarnings = F)

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
  cat(paste0('### RESULTADO 1 de 5: El mejor modelo es ',modelos.mejor,' y se comprueba si ya existe archivo GeoTIFF de prediccion ###','\n'))
  ##### end output messages ####

  get(load(paste0(modelos.entrada,'/',modelos.mejor,".rds",sep="")))

  #vimp <- data.frame(variables=predictors(modelo.ajuste),
  #                 importance=as.vector(modelo.ajuste$variable.importance))


  pred <- predict(modelo.ajuste, test.data)

  if (is(train.data[,'target'],'numeric')){
    table.results <- data.frame(obs=test.data$target,mod=pred)

    ## Metrica
    ## Fuente: https://github.com/davidcarslaw/ggopenair/blob/aa7c93487c4f0fdc84bbf6a7a0080f93fc39b395/R/modStats.R
    # COE - Coefficient Of Efficiency
    COE <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <-  1 - sum(abs(x[[mod]] - x[[obs]])) / sum(abs(x[[obs]] - mean(x[[obs]])))
        return(round(res, 2))
    }
    COE_val <- COE(table.results)
    print(COE_val)

    #IOA - Index of Agreement
    IOA <- function(x, mod = "mod", obs = "obs"){
      x <- na.omit(x[ , c(mod, obs)])
      LHS <- sum(abs(x[[mod]] - x[[obs]]))
      RHS <- 2 * sum(abs(x[[obs]] - mean(x[[obs]])))
      if (LHS <= RHS) res <- 1 - LHS / RHS else res <- RHS / LHS - 1
        return(round(res, 2))
    }
    IOA_val <- IOA(table.results)
    print(IOA_val)

    # AVE - Amount of Variance Explained
    AVE <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <-  1 - sum((x[[mod]]-x[[obs]])^2, na.rm=TRUE)/ sum((x[[obs]] - mean(x[[obs]], na.rm = TRUE))^2,  na.rm = TRUE)
        return(round(res, 2))
    }
    AVE_val <- AVE(table.results)
    print(AVE_val)

    ## RMSE - Root Mean Square Error
    RMSE <- function(x, mod = "mod", obs = "obs") {
        x <- na.omit(x[ , c(mod, obs)])
        res <- mean((x[[mod]] - x[[obs]]) ^ 2) ^ 0.5
        return(round(res, 2))
    }
    RMSE_val <- RMSE(table.results)
    print(RMSE_val)

    RSR = hydroGOF::rsr(test.data$target,pred)
    print(round(RSR, 2))

    ##usando libreria hydroGOF
    #IOA_val = hydroGOF::d(test.data$target,pred)
    #print(round(IOA_val, 2))
    #
    #RMSE_val = hydroGOF::rmse(test.data$target,pred)
    #print(round(RMSE_val, 2))
    #
    #RSR = hydroGOF::rsr(test.data$target,pred)
    #print(round(RSR, 2))

    incertidumbre.grafico.pairplot <- paste0(modelos.incertidumbre.figuras,'/pairplot.png')
    incertidumbre.grafico.residuales <- paste0(modelos.incertidumbre.figuras,'/residuales.png')

    if (!file.exists(incertidumbre.grafico.pairplot)){
      pairplot <- ggplot(data.frame("target"=test.data$target, "pred"=pred), aes(target, pred)) +
      geom_point() +
      geom_abline(slope=1, intercept=0, color = 'red', linetype = 'dashed', size = 0.6) +
      labs(x = paste0('Observed ',VarObj), y = paste0('Predicted ',VarObj)) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      #xlim(0, 11) + ylim(0, 11) +
      theme_bw() +
      theme(text=element_text(size=18))

      png(file = incertidumbre.grafico.pairplot, width = 700, height = 600)
      print(pairplot)
      dev.off()
    }
    if (!file.exists(incertidumbre.grafico.residuales)){
      residuals <- ggplot(data.frame("target"=test.data$target, "residual"=test.data$target - pred),
           aes(target, residual)) +
      geom_point() +
      geom_abline(slope=0, intercept=0, color = 'red', linetype = 'dashed', size = 0.6) +
      labs(x = paste0('Observed ',VarObj), y = paste0('Residuals ',VarObj)) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(labels = scales::comma) +
      theme_bw() +
      theme(text=element_text(size=18))

      png(file = incertidumbre.grafico.residuales, width = 700, height = 600)
      print(residuals)
      dev.off()
    }

    ## Quantile regression forest
    ##test
    #limite_shp <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba')
    #COV <- crop(COV,limite_shp)
    no_cores <- detectCores() - 1

    # Mapa de incertidumbre #
    modelo.archivo = paste0(modelos.incertidumbre.modelo,'/modelo.rds')
    if (!file.exists(modelo.archivo) ){
      beginCluster(no_cores)
      if (modelos.mejor == 'RandomForest'){
        modelo.ajuste <- quantregForest(y=test.data$target - pred, x=test.data[,predictors(rfmodel)[1:rfe_lim]], ntree=500, keep.inbag=TRUE, mtry=modelo.ajuste$bestTune$mtry)
      } else{
        modelo.ajuste <- quantregForest(y=test.data$target - pred, x=test.data[,predictors(rfmodel)[1:rfe_lim]], ntree=500, keep.inbag=TRUE)
      }
      save(modelo.ajuste, file=modelo.archivo)
      endCluster()
    } else {
      get(load(modelo.archivo))
    }

    # Crear predicciones de la incertidumbre (desviación estandar y error medio)
    incertidumbre.raster.unc <- paste0(modelos.incertidumbre.raster,'/incertidumbre_residuales_std.tif')
    incertidumbre.raster.mean <- paste0(modelos.incertidumbre.raster,'/incertidumbre_residuales_media.tif')

    #if (!file.exists(incertidumbre.raster.unc) ){
    #  beginCluster(no_cores)
    #  clusterR(COV[[predictors(rfmodel)[1:rfe_lim]]], predict, args=list(model=modelo.ajuste,what=sd), filename = incertidumbre.raster.unc, options=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite=TRUE)
    #  endCluster()
    #}
    #if (!file.exists(incertidumbre.raster.mean) ){
    #  beginCluster(no_cores)
    #  clusterR(COV[[predictors(rfmodel)[1:rfe_lim]]], predict, args=list(model=modelo.ajuste,what=mean), filename = incertidumbre.raster.mean, options=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite=TRUE)
    #  endCluster()
    #}

    #plot viz 1
    #https://github.com/paleolimbot/ggspatial
    #plot viz 2
    # https://stackoverflow.com/questions/49910270/r-plot-raster-colorscheme-not-full-range

    incertidumbre.raster.unc.mapa <- paste0(modelos.incertidumbre.figuras,'/',gsub('.tif','.png',basename(incertidumbre.raster.unc)))
    incertidumbre.raster.mean.mapa <- paste0(modelos.incertidumbre.figuras,'/',gsub('.tif','.png',basename(incertidumbre.raster.mean)))

    if (!file.exists(incertidumbre.raster.unc.mapa) ){
      r <- raster(incertidumbre.raster.unc)

      p <- ggplot() +
      annotation_map_tile(zoomin = -1) +
      layer_spatial(r, aes(fill = stat(band1)), alpha = 0.7) +
      scale_fill_viridis_c(name='SDE',na.value = NA, direction=-1, option="inferno", alpha = 0.7) +
      annotation_scale(location = "tl") +
      annotation_north_arrow(location = "br", which_north = "true")

      png(file = incertidumbre.raster.unc.mapa, width = 700, height = 600)
      print(p)
      #print(plot(r, main = paste0('Incertidumbre (desviación estandar) - ',VarObj), col=pal))
      dev.off()
    }
    if (!file.exists(incertidumbre.raster.mean.mapa) ){
      r <- raster(incertidumbre.raster.mean)
      p <- ggplot() +
      annotation_map_tile(zoomin = -1) +
      layer_spatial(r, aes(fill = stat(band1)), alpha = 0.7) +
      scale_fill_viridis_c(name='ME',na.value = NA, direction=-1, option="inferno", alpha = 0.7) +
      annotation_scale(location = "tl") +
      annotation_north_arrow(location = "br", which_north = "true")

      png(file = incertidumbre.raster.mean.mapa, width = 700, height = 600)
      print(p)
      #print(plot(r, main = paste0('Incertidumbre (promedio) - ',VarObj), col=pal))
      dev.off()
    }
  } else if (is(train.data[,'target'],'factor')){

    accuracy <- caret::confusionMatrix(pred,test.data$target)$overall["Accuracy"]
    kappa <- caret::confusionMatrix(pred,test.data$target)$overall["Kappa"]
    print(accuracy)
    print(kappa)

    proba.archivo.geotiff <- paste0(modelos.incertidumbre.raster,'/',str_replace(VarObj,'[.]','-'),'_PROB_',modelos.mejor,'.tif')
    if (!file.exists(proba.archivo.geotiff)){
      index <- 1:nlevels(train.data[['target']])
      type <- 'prob'
      ##### output messages ####
      cat(paste0('### RESULTADO 2 de 5: El archivo GeoTIFF de probabilidad usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', proba.archivo.geotiff,' ###','\n'))
      ##### end output messages ####
      start <- Sys.time()
      PredictGeoTIFF(COV, modelo.ajuste, proba.archivo.geotiff, type, index, train.data)
      print(Sys.time() - start)
    } else {
      cat(paste0('### RESULTADO 2 de 5: El archivo GeoTIFF de probabilidad usando mejor modelo ',modelos.mejor,' SI existe y se encuentra en la ruta ', proba.archivo.geotiff,' ###','\n'))
    }

    entropia.archivo.geotiff <- gsub('_PROB_','_ENTROPIA_',proba.archivo.geotiff)
    if (!file.exists(entropia.archivo.geotiff)){
      ##### output messages ####
      cat(paste0('### RESULTADO 3a de 5: El archivo GeoTIFF de entropia usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', entropia.archivo.geotiff,' ###','\n'))
      ##### end output messages ####
      entropy <- function (x) {
        - sum(x * log(x, base = length(x)), na.rm = TRUE)
      }

      prediction_prob <- stack(proba.archivo.geotiff)

      start <- Sys.time()
      no_cores <- detectCores() - 1
      beginCluster(no_cores)
      clusterR(prediction_prob, fun = calc, args = list(fun = entropy),
                            filename = entropia.archivo.geotiff, format = "GTiff",
                            overwrite = T, datatype='FLT4S', options='COMPRESS=YES')
      endCluster()
      print(Sys.time() - start)

    } else {
      cat(paste0('### RESULTADO 3a de 5: El archivo GeoTIFF de entropia usando mejor modelo ',modelos.mejor,' SI existe y se encuentra en la ruta ', entropia.archivo.geotiff,' ###','\n'))
    }

    confusion.archivo.geotiff <- gsub('_PROB_','_CONFUSION_',proba.archivo.geotiff)
    if (!file.exists(confusion.archivo.geotiff)){
      ##### output messages ####
      cat(paste0('### RESULTADO 3b de 5: El archivo GeoTIFF del índice de confusión usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', confusion.archivo.geotiff,' ###','\n'))
      ##### end output messages ####
      # Function to compute the confusion index ----
      confusion <- function (x) {
          1 - diff(sort(x, decreasing = TRUE)[2:1])
        }

      start <- Sys.time()
      prediction_prob <- stack(proba.archivo.geotiff)

      no_cores <- detectCores() - 1
      beginCluster(no_cores)
      clusterR(prediction_prob, fun = calc, args = list(fun = confusion),
                            filename = confusion.archivo.geotiff, format = "GTiff",
                            overwrite = T, datatype='FLT4S', options='COMPRESS=YES')
      endCluster()
      print(Sys.time() - start)

    } else {
      cat(paste0('### RESULTADO 3b de 5: El archivo GeoTIFF del índice de confusión usando mejor modelo ',modelos.mejor,' SI existe y se encuentra en la ruta ', confusion.archivo.geotiff,' ###','\n'))
    }

    proba.archivo.grafica <- paste0(modelos.incertidumbre.figuras,'/',gsub('.tif$','.png',basename(proba.archivo.geotiff)))
    if (!file.exists(proba.archivo.grafica)){
      ##### output messages ####
      cat(paste0('### RESULTADO 4 de 5: La figura de probabilidades de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', proba.archivo.grafica,' ###','\n'))
      ##### end output messages ####
      r <- stack(proba.archivo.geotiff)
      
      n<-100
      
      colr = viridis::viridis(n, direction=-1, begin = 0, end = 1)
      names(r) <- levels(train.data$target)
      titles <- gsub('_','\n',levels(train.data$target))
      #titles <- gsub("\\b([A-Z])(\\w+)", "\\1\\L\\2", titles, perl = TRUE)
      #print(titles)
      
      dev.new(height=0.91*nrow(r)/50, width=1.09*ncol(r)/50)
      png(file = proba.archivo.grafica, width = 1200, height = 900, res=150)
      p <- levelplot(r, panel=panel.levelplot.raster, margin=T, layout=c(8, 3), names.attr=titles, 
                at=seq(0, 1, length.out=n), col.regions = colr,
                par.strip.text=list(font=2, cex=0.5, lines=6), 
                par.settings = list(axis.line = list(col = 'transparent'),
                                    strip.background = list(col = 'transparent'),
                                    strip.border = list(col = 'transparent')),
                scales = list(col = 'transparent'), colorkey=list(space="bottom",  height = 1, width = 1))
      print(p)
      #grid::grid.text('Probabilidad', rot=0, y=unit(0.1, "npc"), x=unit(0.5, "npc"))
      dev.off()
      
      # dev.new(height=0.91*nrow(r)/50, width=1.09*ncol(r)/50)
      # png(file = proba.archivo.grafica, width = 1400, height = 600, res=150)
      # plot(r, cex.main=0.5,  legend=FALSE, zlim=c(0, 1), col=colr, axes=FALSE, box=FALSE, bty='L', nc=7, maxnl=length(names(r)))
      # #plot(r, cex.main=0.6, zlim=c(0, 1), col=colr, axes=FALSE, box=FALSE, legend.args = list(text = 'Probabilidad', side = 4, font = 2, line = 2.5, cex = 0.5), nc=7, maxnl=length(names(r)))
      # #legend(0.03, 0.025,c("-50","-25","0","50","100"), lty=c(6,6,1,1,1),lwd=6,col=colr,cex=2,title=expression(paste(4,N,'s',sep="")),bty="n")
      # #plot(r, legend.only=TRUE, col=colr, legend.width=5, legend.shrink=1,legend.args=list(text='Probabilidad', side=2, font=1, line=8, cex=1), axis.args=list(cex.axis=1))
      # #plot(r, col=colr, horizontal=TRUE, breaks=seq(0,1,length.out=100), legend.only=TRUE, legend.shrink = 1, legend.width = 3, axis.args = list(at=pretty(0:1), labels=pretty(0:1), cex.axis=0.2), legend.args= list(text='Probabilidad', side=1, font=3, line = 2, cex=1))
      # #legend(0.03, 0.025, legend = unique(rfp.dt$climate), fill = colr, cex = 0.8, lwd = 1, lty = 1)
      # dev.off()
      # 
      # proba.archivo.grafica.leyenda <- paste0(modelos.incertidumbre.figuras,'/',gsub('.tif$','_leyenda.png',basename(proba.archivo.geotiff)))
      # png(file = proba.archivo.grafica.leyenda, width = 300, height = 50, res=150)
      # plot(r,legend.only=TRUE, horizontal=TRUE, legend.shrink=1, legend.width=1, zlim=c(0, 1),
      #      axis.args=list(at=pretty(0:1), labels=pretty(0:1)),
      #      legend.args=list(text='Probabilidad', font=2, side=1))
      # dev.off()
      
    } else{
      ##### output messages ####
      cat(paste0('### RESULTADO 4 de 5: La figura de probabilidades de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', proba.archivo.grafica,' ###','\n'))
      ##### end output messages ####
    }

    entropia.archivo.grafica <- paste0(modelos.incertidumbre.figuras,'/',gsub('.tif$','.png',basename(entropia.archivo.geotiff)))
    if (!file.exists(entropia.archivo.grafica)){
      ##### output messages ####
      cat(paste0('### RESULTADO 5a de 5: La figura de entropia de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', entropia.archivo.grafica,' ###','\n'))
      ##### end output messages ####

      r <- raster(entropia.archivo.geotiff)

      limite_shp <- st_read(paste0(in.geo.data,'/vector/limite'))
      if (dim(limite_shp)[1] > 1){
         limite_shp$id <- 0
         limite_shp <- limite_shp %>% group_by(id) %>% summarize()
      }
      
      r_res <- crop(r,limite_shp)
      r_res <- mask(r_res,limite_shp)

      r <- r_res
      p <- ggplot() +
        annotation_map_tile(zoomin = -1) +
        layer_spatial(r, aes(fill = stat(band1)), alpha = 0.7) +
        scale_fill_viridis_c(name='Entropia',na.value = NA, direction=-1, option="inferno", alpha = 0.7,  limits = c(0, 1)) +
        annotation_scale(location = "tl") +
        annotation_north_arrow(location = "br", which_north = "true")
      
      png(file = entropia.archivo.grafica, width = 700, height = 600, res=150)
      print(p)
      #print(plot(r, main = paste0('Incertidumbre (promedio) - ',VarObj), col=pal))
      dev.off()
      # 
      
      # colr = viridis::inferno(100, direction=-1, begin = 0, end = 1)
      # dev.new(height=0.91*nrow(r)/50, width=1.09*ncol(r)/50)
      # png(file = entropia.archivo.grafica, width = 1400, height = 900, res=150)
      # plot(r_res, zlim=c(0, 1), col=colr, axes=FALSE, box=FALSE)
      # dev.off()
    } else{
      ##### output messages ####
      cat(paste0('### RESULTADO 5a de 5: La figura de entropia de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', entropia.archivo.grafica,' ###','\n'))
      ##### end output messages ####
    }

    confusion.archivo.grafica <- paste0(modelos.incertidumbre.figuras,'/',gsub('.tif$','.png',basename(confusion.archivo.geotiff)))
    if (!file.exists(confusion.archivo.grafica)){
      ##### output messages ####
      cat(paste0('### RESULTADO 5b de 5: La figura del índice de confusión de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' NO existe y se esta generando en la ruta ', confusion.archivo.grafica,' ###','\n'))
      ##### end output messages ####

      r <- raster(confusion.archivo.geotiff)
      
      p <- ggplot() +
        annotation_map_tile(zoomin = -1) +
        layer_spatial(r, aes(fill = stat(band1)), alpha = 0.7) +
        scale_fill_viridis_c(name='Confusi�n',na.value = NA, direction=-1, option="inferno", alpha = 0.7, limits = c(0, 1)) +
        annotation_scale(location = "tl") +
        annotation_north_arrow(location = "br", which_north = "true")
      
      png(file = confusion.archivo.grafica, width = 700, height = 600, res=150)
      print(p)
      #print(plot(r, main = paste0('Incertidumbre (promedio) - ',VarObj), col=pal))
      dev.off()
      # 
      # colr = inferno(100, direction=-1, begin = 0, end = 1)
      # 
      # dev.new(height=0.91*nrow(r)/50, width=1.09*ncol(r)/50)
      # png(file = confusion.archivo.grafica, width = 1400, height = 900, res=150)
      # plot(r, zlim=c(0, 1), col=colr, axes=FALSE, box=FALSE)
      # dev.off()
    } else{
      ##### output messages ####
      cat(paste0('### RESULTADO 5a de 5a: La figura del índice de confusión de la variable ',str_replace(VarObj,'[.]','-'),' usando mejor modelo ',modelos.mejor,' existe y se encuentra en la ruta ', confusion.archivo.grafica,' ###','\n'))
      ##### end output messages ####
    }
  }

  # LIME
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