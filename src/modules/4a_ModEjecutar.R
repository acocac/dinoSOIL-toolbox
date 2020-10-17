#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModEntrenamiento <- function(VarObj){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  #pckg <- c('raster', 'rgdal', 'caret', 'doMC', 'plyr', 'doParallel',
  #          'dismo', 'readxl', 'aqp', 'sp', 'randomForest', 'RWeka', 'rJava',
  #          'C50')
  pckg <- c('raster', 'rgdal', 'caret', 'doMC', 'plyr', 'doParallel',
            'dismo', 'readxl', 'aqp', 'sp', 'ranger', 'stringr', 'h2o')

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
  project.name <- sapply(strsplit(proyecto.directorio, '_'), tail, 1)
  proyecto.modelos.categoricas <- conf.args[[5]]
  proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))
  proyecto.modelos.continuas <- conf.args[[6]]
  proyecto.modelos.continuas = unlist(strsplit(proyecto.modelos.continuas,';'))

  # Modelos disponibles y configuraciones
  modelos.lista <-c('C45'='J48', 'C50'='C5.0', 'RandomForest'='ranger', 'SVM'='svmLinear',
                    'xgbTree'='xgbTree','gbm'='gbm_h2o', 'glmnet'='glmnet','mlp'='mlp')
  tuneLenght <-c('C45'=5, 'C50'=5, 'RandomForest'=5, 'SVM'=5, 'xgbTree'=5, 'gbm_h2o'=3,
                 'glmnet'=5,'mlp'=5)

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/0_particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.entrada, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/1_modelos/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.salida, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particición
  load(paste0(modelos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  test.data <- particion['test']

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
    fm <- as.formula(paste("target~", paste0(as.character(predictors(rfmodel)[c(1:3)]),collapse = "+"))) #TODO dejar número variables según usuario

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

      modelo.ajuste <- train(fm, data = train.data,
      method=modelos.lista[modelo],
      tuneLength = tuneLenght[modelo],
      metric='Rsquared',
      trControl = fitControl)

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
      # As a rule of thumb, a class to be modelled should have at least 5 observations
      # source: https://soilmapper.org/soilmapping-using-mla.html
  }
}