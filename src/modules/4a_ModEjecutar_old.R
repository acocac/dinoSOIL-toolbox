#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ModEntrenamiento <- function(){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg <- c('raster', 'rgdal', 'caret', 'doMC', 'plyr', 'doParallel',
            'dismo', 'readxl', 'aqp', 'sp', 'randomForest', 'RWeka', 'rJava',
            'C50')

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
  proyecto.modelos.categoricas <- conf.args[[4]]
  proyecto.modelos.categoricas = unlist(strsplit(proyecto.modelos.categoricas,';'))
  proyecto.modelos.continuas <- conf.args[[5]]
  proyecto.modelos.continuas = unlist(strsplit(proyecto.modelos.continuas,';'))

  # Modelos disponibles y configuraciones
  modelos.lista <-c('C45'='J48', 'C50'='C5.0', 'RandomForest'='rf', 'SVM'='svmLinear')
  tuneLenght <-c('C45'=5, 'C50'=5, 'RandomForest'=30, 'SVM'=5)

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  datos.entrada <- paste0(proyecto.directorio,'/datos/entrada/1_covariables')
  datos.salida <- paste0(proyecto.directorio,'/datos/salida/1_predicciones')
  dir.create(datos.salida, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/rds')
  dir.create(modelos.salida, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar 1_covariables
  COV <- stack(paste0(datos.entrada,'/color/raster/COV_VF.tif'))
  names(COV) <- readRDS(paste0(datos.entrada,'/color/rds/NAMES_COV_VF_COLOR.rds'))

  # Cargar matrix observaciones
  dat_subset <- read.csv(paste0(datos.entrada,'/color/tabular/RegMatrix_VF_observaciones.csv'),sep=',')

  # Remover NAs
  dat_subset <- na.omit(dat_subset)

  if (is(matriz[,VarObj],'numeric')){

  # Determinar modelos objetivo segun listado en el archivo conf.txt y modelos disponibles por categoria
  modelos.idx <- match(proyecto.modelos, names(modelos.lista))
  modelos.objetivo <- modelos.lista[modelos.idx]

  # Factorizar
  dat_subset$orden <- factor(dat_subset$orden)
  dat_subset$Cobertura <- factor(dat_subset$Cobertura)
  dat_subset$dip_1<- factor(dat_subset$dip_1)
  dat_subset$naf_1 <- factor(dat_subset$naf_1)
  dat_subset$dip_2 <- factor(dat_subset$dip_2)
  dat_subset$naf_2 <- factor(dat_subset$naf_2)
  dat_subset$T_Relieve <- factor(dat_subset$T_Relieve)
  dat_subset$F_TERRENO <- factor(dat_subset$F_TERRENO)
  dat_subset$MAT_PARENTAL <- factor(dat_subset$MAT_PARENTAL)

  # ------------------------------------------------------- #
  # Modelos
  # ------------------------------------------------------- #
  # Crear formula
  fm <- as.formula(paste("orden~", paste0(names(COV)[c(23, 6, 34, 9, 3, 8, 10, 7, 21)], collapse = "+")))
  #print(paste0(names(COV)[c(23, 6, 34, 9, 3, 8, 10, 7, 21)],collapse='-'))
  #print('MODELAR: Orden con las 1_covariables :',names(COV)[c(23, 6, 34, 9, 3, 8, 10, 7, 21)])

  #Random grid search
  fitControl <- trainControl(method="repeatedcv", number = 3,
                             repeats = 3, classProbs = TRUE,
                             savePredictions = TRUE, search="random", allowParallel= TRUE,
                             verboseIter = FALSE)

  #execute the algorithms
  for (modelo in names(modelos.objetivo)){
    modelo.archivo <- paste0(modelos.salida, '/',modelo,'.rds')
    if (!file.exists(modelo.archivo)){
      cat(paste0('El modelo ',modelo,' no existe, se requiere entrenarlo antes de su evaluacion', '\n'))

      # Calculate the number of cores
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
      registerDoParallel(cl)                #register the cluster

      ## foreach or lapply would do this faster
      set.seed(40)
      modelo.ajuste <- train(fm, data = dat_subset,
                        method=modelos.lista[modelo],
                        metric = 'Kappa',
                        maximize = TRUE,
                        tuneLength = tuneLenght[modelo],
                        trControl = fitControl)

      stopCluster(cl = cl)

      if(modelo == 'C45'){
        .jcache(modelo.ajuste$finalModel$classifier)
      }
      save(modelo.ajuste, file=modelo.archivo)
    } else {
      cat(paste0('El modelo ',modelo,' existe, se ve a cargar para prediccion','\n'))
      #modelo.ajuste <- load(modelo.archivo)
    }
  }
}
  ## Traditional manual grid search by model
  #if (all(project.modelos == lista.modelos[1])){
  #  nombre.modelo <- lista.modelos[1]
  #  model.file <- paste0(out.modelos, '/',nombre.modelo,'.rds')
  #  if (!file.exists(model.file)){
  #    cat(paste0('El modelo ',nombre.modelo,' no existe, se requiere entrenarlo antes de prediccion'))
  #    ################################################################################
  #    ### Section 2: Modelo RF
  #    ## Section 2a: Configuraciones
  #    # Definir estrategia de entrenamiento, 10-fold cross-validation
  #    ctrl <- trainControl(method = "cv", savePred=T)
  #
  #    # Entrenar modelo
  #    no_cores <- detectCores() - 1
  #    cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
  #    registerDoParallel(cl)                #register the cluster
  #    set.seed(40)
  #    fit.model <- train(fm, data=dat_subset, method = 'rf', trControl = ctrl, importance=TRUE)
  #    stopCluster(cl = cl)
  #
  #    # Salvar el modelo
  #    saveRDS(fit.model, model.file)
  #  } else {
  #    cat(paste0('El modelo ',nombre.modelo,' existe, se ve a cargar para prediccion'))
  #    fit.model <- readRDS(model.file)
  #  }
  #  PredictGeoTIFF(COV, fit.model, out.data, nombre.modelo)
  #  cat(paste0('### RESULTADO: El modelo ',nombre.modelo,' fue entrenado para el proyecto ',basename(project.name)))
  #
  #} else if (all(project.modelos == lista.modelos[2])){
  #  nombre.modelo <- lista.modelos[2]
  #  model.file <- paste0(out.modelos,'/',nombre.modelo,'.rds')
  #  if (!file.exists(model.file)){
  #    cat(paste0('El modelo ',nombre.modelo,' no existe, se requiere entrenarlo antes de prediccion'))
  #    ################################################################################
  #    ### Section 1: Modelo C4.5
  #    ## Section 1a: Configuraciones
  #    C = seq(0.05, 0.5,by=0.05)
  #    M = seq(1, 30,by=1)
  #
  #    #tuning grid for train caret function
  #    my.grid <- expand.grid(.C = C, .M = M)
  #    n.repeats <- 3
  #    n.resampling <- 2
  #
  #    #create a list of seed, here change the seed for each resampling
  #    set.seed(40)
  #    length.seeds = (n.repeats*n.resampling) + 1
  #    n.tune.parameters = length(C)*length(M)
  #    seeds <- vector(mode = "list", length = length.seeds)#length is = (n_repeats*nresampling)+1
  #    for(i in 1:length.seeds) seeds[[i]]<- sample.int(n=1000, n.tune.parameters) #(n.tune.parameters = number of tuning parameters)
  #    seeds[[length.seeds]]<-sample.int(1000, 1)#for the last model
  #
  #    #create a control object for the models, implementing 10-crossvalidation repeated 10 times
  #    fit.Control <- trainControl(
  #      method = "repeatedcv",
  #      number = n.resampling, ## k-fold CV
  #      repeats = n.repeats, ## iterations
  #      savePred = TRUE,
  #      seeds = seeds
  #    )
  #
  #    ## Section 1b: Correr
  #    ## parallel process ##
  #    #cluster
  #    # Calculate the number of cores
  #    no_cores <- detectCores() - 1
  #    cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
  #    registerDoParallel(cl)                #register the cluster
  #
  #    ## foreach or lapply would do this faster
  #    set.seed(40)
  #    fit.model <- train(fm,
  #                       data=dat_subset,
  #                       trControl = fit.Control,
  #                       preProcess=c("range"),
  #                       method = "J48",
  #                       metric = "Kappa",
  #                       tuneGrid = my.grid
  #
  #    )
  #
  #    stopCluster(cl = cl)
  #
  #    #
  #    ##export and store model results
  #    .jcache(fit.model$finalModel$classifier)
  #    #save(fit.model, file=model.file)
  #    saveRDS(fit.model, file=model.file)
  #
  #    } else {
  #      cat(paste0('El modelo ',nombre.modelo,' existe, se ve a cargar para prediccion'))
  #      fit.model <- readRDS(model.file)
  #    }
  #  PredictGeoTIFF(COV, fit.model, out.data, nombre.modelo)
  #}
#}