#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModEntrenamiento <- function(VarObj, BaseDatos, rfe_lim, Muestreo){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  #pckg <- c('raster', 'rgdal', 'caret', 'doMC', 'plyr', 'doParallel',
  #          'dismo', 'readxl', 'aqp', 'sp', 'ranger', 'stringr', 'h2o', 'RWeka')

  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(raster, rgdal, caret, stringr))

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
  project.name <- sapply(strsplit(proyecto.directorio, '_'), tail, 1)
  proyecto.modelos.categoricas <- conf.args[[5]]
  proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))
  proyecto.modelos.continuas <- conf.args[[6]]
  proyecto.modelos.continuas = unlist(strsplit(proyecto.modelos.continuas,';'))

  # Modelos disponibles y configuraciones
  modelos.lista <-c('C45'='J48', 'C50'='C5.0', 'RandomForest'='ranger', 'svmLinear'='svmLinear',
                    'xgbTree'='xgbTree','gbm'='gbm_h2o', 'glmnet'='glmnet','mlp'='mlp', 'svmRadial'='svmRadial')
  tuneLenght <-c('C45'=5, 'C50'=5, 'RandomForest'=20, 'SVM'=5, 'xgbTree'=20, 'gbm_h2o'=3,
                 'glmnet'=5,'mlp'=5, 'svmRadial'=20)

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/',BaseDatos,'/rds/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.entrada, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/2_modelos/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(modelos.salida, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particición
  load(paste0(modelos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub("train.", "", names(train.data))

  # Cargar rfe
  load(paste0(exploratorio.variables,'/rfe.rds'))

  if (is(train.data[,'target'],'numeric')){
    # Determinar modelos objetivo segun listado en el archivo conf.txt y modelos disponibles por categoria
    modelos.idx <- match(proyecto.modelos.continuas, names(modelos.lista))
    modelos.objetivo <- modelos.lista[modelos.idx]

    # ------------------------------------------------------- #
    # Modelos
    # ------------------------------------------------------- #
    # Crear formula
    fm <- as.formula(paste("target~", paste0(as.character(predictors(rfmodel)[c(1:rfe_lim)]),collapse = "+"))) #TODO dejar número variables según usuario

    #Random grid search
    fitControl <- trainControl(method = "cv", #verificar tecnicas repeatedcv
                          number=10,
                          returnResamp = "all",
                          savePredictions = TRUE,
                          search = "random",
                          verboseIter = FALSE
    )

    #execute the algorithms
    for (modelo in names(modelos.objetivo)){
      modelo.archivo <- paste0(modelos.salida, '/',modelo,'.rds')
    if (!file.exists(modelo.archivo)){
      cat(paste0('El modelo ',modelo,' no existe, se requiere entrenarlo antes de su evaluacion','\n','\n'))

      # Calculate the number of cores
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
      registerDoParallel(cl)                #register the cluster

      ## foreach or lapply would do this faster
      set.seed(40)

      if (modelo == 'RandomForest'){
        modelo.ajuste <- train(fm, data = train.data,
        method=modelos.lista[modelo],
        tuneLength = tuneLenght[modelo],
        num.trees = 500,
        metric='RMSE',
        trControl = fitControl)
      } else{
        modelo.ajuste <- train(fm, data = train.data,
        method=modelos.lista[modelo],
        tuneLength = tuneLenght[modelo],
        metric='RMSE',
        preProc = c("center", "scale"),
        trControl = fitControl)
      }

      stopCluster(cl = cl)

      if(modelo == 'C45'){
        .jcache(modelo.ajuste$finalModel$classifier)
      }
        save(modelo.ajuste, file=modelo.archivo)
      } else {
        cat(paste0('El modelo ',modelo,' existe, se ve a cargar para prediccion','\n','\n'))
      }
      }
      }
  else if (is(train.data[,'target'],'factor')){
    # Determinar modelos objetivo segun listado en el archivo conf.txt y modelos disponibles por categoria
    modelos.idx <- match(proyecto.modelos.categoricas, names(modelos.lista))
    modelos.objetivo <- modelos.lista[modelos.idx]

    # ------------------------------------------------------- #
    # Modelos
    # ------------------------------------------------------- #
    # Crear formula
    fm <- as.formula(paste("target~", paste0(as.character(predictors(rfmodel)[c(1:rfe_lim)]),collapse = "+"))) #TODO dejar número variables según usuario
    #fm <- as.formula(paste("Class~", paste0(as.character(predictors(rfmodel)[c(1:rfe_lim)]),collapse = "+"))) #TODO dejar número variables según usuario

      #for (sampling_type in c('original')){
      if (tolower(Muestreo) %in% c('up','down','original')){
        
        sampling_type <- tolower(Muestreo) 
      
        modelos.salida.temp <- paste0(modelos.salida,'/',toupper(sampling_type))
        dir.create(modelos.salida.temp, recursive = T, mode = "0777", showWarnings = F)

        if (sampling_type != 'original') {
          #Random grid search
          fitControl <- trainControl(method = "cv", #verificar tecnicas repeatedcv
                                number=10,
                                classProbs = TRUE,
                                summaryFunction = multiClassSummary,
                                returnResamp = "all",
                                savePredictions = TRUE,
                                search = "random",
                                verboseIter = FALSE,
                                sampling = sampling_type
          )
        } else{
          #Random grid search
          fitControl <- trainControl(method = "cv", #verificar tecnicas repeatedcv
                                number=10,
                                classProbs = TRUE,
                                summaryFunction = multiClassSummary,
                                returnResamp = "all",
                                savePredictions = TRUE,
                                search = "random",
                                verboseIter = FALSE
          )
        }

        #execute the algorithms
        for (modelo in names(modelos.objetivo)){
          modelo.archivo <- paste0(modelos.salida.temp,'/',modelo,'.rds')
          if (!file.exists(modelo.archivo)){
            cat(paste0('El modelo ',modelo,' con sampling ',toupper(sampling_type),' no existe, se requiere entrenarlo antes de su evaluacion','\n','\n'))

            # Calculate the number of cores
            no_cores <- detectCores() - 1
            cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
            registerDoParallel(cl)                #register the cluster

            ## foreach or lapply would do this faster
            set.seed(40)

            if (modelo == 'RandomForest'){
              modelo.ajuste <- train(fm, data = train.data,
              method=modelos.lista[modelo],
              tuneLength = tuneLenght[modelo],
              num.trees = 500,
              metric='Accuracy',
              trControl = fitControl)
            } else{
              modelo.ajuste <- train(fm, data = train.data,
              method=modelos.lista[modelo],
              tuneLength = tuneLenght[modelo],
              metric='Accuracy',
              preProc = c("center", "scale"),
              trControl = fitControl)
            }

            stopCluster(cl = cl)

            if(modelo == 'C45'){
              .jcache(modelo.ajuste$finalModel$classifier)
            }
              save(modelo.ajuste, file=modelo.archivo)
            } else {
              cat(paste0('El modelo ',modelo,' con sampling ',toupper(sampling_type),' existe, se ve a cargar para prediccion','\n','\n'))
            }
            }
      } else {
        print('no esta')
      }
  }
}