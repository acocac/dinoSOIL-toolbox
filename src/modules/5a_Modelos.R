#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

Entrenamiento <- function(){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg <- c('raster', 'rgdal', 'caret', 'doMC', 'plyr', 'doParallel',
            'dismo', 'readxl', 'aqp', 'sp', 'randomForest')

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
  project.folder <- conf.args[[1]]
  project.name <- sapply(strsplit(project.folder, '_'), tail, 1)

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  in.data <- paste0(project.folder,'/datos/entrada/1_covariables')
  out.data <- paste0(project.folder,'/datos/salida/1_predicciones')
  dir.create(out.data, recursive = T, mode = "0777", showWarnings = F)
  out.modelos <- paste0(project.folder,'/modelos')

  # Definir directorio de trabajo
  setwd(paste0(project.folder))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar covariables
  COV <- stack(paste0(in.data,'/color/raster/COV_VF.tif'))
  names(COV) <- readRDS(paste0(in.data,'/color/rds/NAMES_COV_VF_COLOR.rds'))

  # Cargar matrix observaciones
  dat_subset <- read.csv(paste0(in.data,'/color/tabular/RegMatrix_VF_observaciones.csv'),sep=',')

  # Remover NAs
  dat_subset <- na.omit(dat_subset)

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

  # Random Forest
  model.file <- paste0(out.modelos, '/randomforest.rds')

  if (!file.exists(model.file)){
    cat('El modelo NO existe, se requiere entrenarlo antes de prediccion')
    # Definir estrategia de entrenamiento, 10-fold cross-validation
    ctrl <- trainControl(method = "cv", savePred=T)

    # Entrenar modelo
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
    registerDoParallel(cl)                #register the cluster
    set.seed(40)
    fit.model <- train(fm, data=dat_subset, method = 'rf', trControl = ctrl, importance=TRUE)
    stopCluster(cl = cl)

    # Salvar el modelo
    saveRDS(fit.model, model.file)
  } else {
    cat('El modelo existe, se ve a cargar para prediccion')
    fit.model <- readRDS(model.file)
  }

  # Evaluar el modelo
  xtab <- table(fit.model$pred[,1], fit.model$pred[,2])
  con.mat <- confusionMatrix(xtab)

  # Predecir en subarea
  COVSub <- crop(COV, extent(COV, 1, 100, 1, 100))
  print(dim(COVSub))
  pred <- predict(COVSub, fit.model, filename = paste0(out.data,'/RF_5m_PRED_orden_18092018_v2.tif'),
                format = "GTiff", overwrite = T)

  # ------------------------------------------------------- #
  # Mensajes de salida
  # ------------------------------------------------------- #
  cat(paste0('### RESULTADO: El modelo Random Forest fue entrenado para el proyecto ',basename(project.name)))

}