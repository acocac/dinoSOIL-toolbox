#############################################################################
# titulo        : Entrenamiento modelos;
# proposito     : Entrenar varios modelos de Aprendizaje de Maquinas;
# autor(es)     : Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Actualizado por ACC en Noviembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones :glmnet ninguna;
##############################################################################

ModEntrenamiento <- function(VarObj, BaseDatos, rfe_lim, Muestreo, listmodelos){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(raster, rgdal, caret, stringr,doParallel, purrr))

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
  project.name <- sapply(strsplit(proyecto.directorio, '_'), tail, 1)
  
  if (listmodelos == 'DEFECTO'){
    configuracion <- modelos.config.defecto()
    proyecto.modelos.continuas <- configuracion[[1]]
    proyecto.modelos.categoricas <- configuracion[[2]]
    tuneLenght <- configuracion[[3]]
    
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
    
    # Modelos disponibles y configuraciones
    configuracion <- modelos.config.manual()
    modelos.lista <- configuracion[[1]]
    tuneLenght <- configuracion[[2]]
  }
  
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
    if (listmodelos %in% c('DEFECTO',tolower('DEFECTO'))){
      modelos.lista = mapply(Add, proyecto.modelos.continuas, proyecto.modelos.continuas)
      modelos.objetivo = modelos.lista
    } else{
      modelos.idx <- match(proyecto.modelos.continuas, names(modelos.lista))
      modelos.objetivo <- modelos.lista[modelos.idx]
    }
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
      modelos.salida.temp <- paste0(modelos.salida, '/', toupper(Muestreo), '/', listmodelos)
      dir.create(modelos.salida.temp, recursive = T, mode = "0777", showWarnings = F)
      modelo.archivo <- paste0(modelos.salida.temp, '/',modelo,'.rds')
    if (!file.exists(modelo.archivo)){
      cat(paste0('El modelo ',modelo,' no existe, se requiere entrenarlo antes de su evaluacion','\n','\n'))

      # Calculate the number of cores
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
      registerDoParallel(cl)                #register the cluster

      ## foreach or lapply would do this faster
      set.seed(40)
      
      model.alias <- modelos.lista[modelo]
      
      if (getModelInfo(model.alias)[[model.alias]]$label[1] %in% c('RandomForest','Random Forest')){
        modelo.ajuste <- train(fm, data = train.data,
        method=modelos.lista[modelo],
        tuneLength = tuneLenght[modelo],
        num.trees = 500,
        metric=proyecto.metricas.continuas[1],
        importance = "impurity",
        trControl = fitControl)
      } else{
        modelo.ajuste <- train(fm, data = train.data,
        method=modelos.lista[modelo],
        tuneLength = tuneLenght[modelo],
        metric=proyecto.metricas.continuas[1],
        importance = TRUE,
        preProc = c("center", "scale"),
        trControl = fitControl)
      }

      stopCluster(cl = cl)

      if(modelo == 'C45'){
        .jcache(modelo.ajuste$finalModel$classifier)
      }
        save(modelo.ajuste, file=modelo.archivo)
      } else {
        cat(paste0('El modelo ',modelo,' existe, se puede usar para identificar mejor modelos','\n','\n'))
      }
      }
      }
  else if (is(train.data[,'target'],'factor')){
    if (listmodelos %in% c('DEFECTO',tolower('DEFECTO'))){
      modelos.lista = mapply(Add, proyecto.modelos.categoricas, proyecto.modelos.categoricas)
      modelos.objetivo = modelos.lista
    } else{
      # Determinar modelos objetivo segun listado en el archivo conf.txt y modelos disponibles por categoria
      modelos.idx <- match(proyecto.modelos.categoricas, names(modelos.lista))
      if (anyNA(modelos.idx)){
        stop('No se continua la ejecuci�n los siguientes modelos del CONFIG: ', paste0(proyecto.modelos.categoricas[which(is.na(modelos.idx))],collapse=', '), ' NO corresponden a los listados en el archivo de configuraciones. Se recomienda verificar si el nombre es correcto.')    
      } else { #continue the script
        modelos.objetivo <- modelos.lista[modelos.idx]
      }
    }

    # ------------------------------------------------------- #
    # Modelos
    # ------------------------------------------------------- #
    # Crear formula
    fm <- as.formula(paste("target~", paste0(as.character(predictors(rfmodel)[c(1:rfe_lim)]),collapse = "+"))) #TODO dejar número variables según usuario
    #fm <- as.formula(paste("Class~", paste0(as.character(predictors(rfmodel)[c(1:rfe_lim)]),collapse = "+"))) #TODO dejar número variables según usuario


        #for (sampling_type in c('original')){
    if (tolower(Muestreo) %in% c('up','down','original')){
        
      sampling_type <- tolower(Muestreo) 
    
      modelos.salida.temp <- paste0(modelos.salida, '/', toupper(sampling_type), '/', listmodelos)
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
                              sampling = sampling_type,
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

          model.alias <- modelos.lista[modelo]

          if (getModelInfo(model.alias)[[model.alias]]$label[1] %in% c('RandomForest','Random Forest')){
            modelo.ajuste <- train(fm, data = train.data,
            method=modelos.lista[modelo],
            tuneLength = tuneLenght[modelo],
            num.trees = 500,
            importance = "impurity",
            metric=proyecto.metricas.categoricas[1],
            trControl = fitControl)
          } else{
            modelo.ajuste <- train(fm, data = train.data,
            method=modelos.lista[modelo],
            tuneLength = tuneLenght[modelo],
            metric=proyecto.metricas.categoricas[1],
            importance = TRUE,
            preProc = c("center", "scale"),
            trControl = fitControl)
          }

          stopCluster(cl = cl)

          if(modelo == 'C45'){
            .jcache(modelo.ajuste$finalModel$classifier)
          }
            save(modelo.ajuste, file=modelo.archivo)
          } else {
            cat(paste0('El modelo ',modelo,' con sampling ',toupper(sampling_type),' existe, se puede usar para identificar mejor modelos','\n','\n'))
          }
          }
    } else {
      print('no esta')
    }
  }
}