#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ExpRFE <- function(VarObj, BaseDatos){
  ##
  ## src1:
  ## https://github.com/m2-rshiny/ProjetTut/blob/426fcff7642ffdd8f84743c88cc732e2bd617ca7/Archives/MLShiny2/analysis-UGA.R
  ## https://github.com/raiajeet/AmExpert-2018-Machine-Learning-Hackathon-/blob/9f9c6711cc5c012bbf5520ebb0c188c0d3847c67/code.R
  ## https://github.com/Edimer/DataSource.ai/blob/226e358b5d106d075b396324956bd28e05d96e51/PreciosApartamentos/codeR/LightGBM2.R
  ## https://github.com/Edimer/Zindi.africa/blob/2802ad05863fdf3b5d52f602d545bc39d8b3affe/Prediction_Flood/R/lgbmR1.R

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  ## Librerias
  #pckg = c('compareGroups','caret','raster',
  #         'doParallel','dplyr','labelled',
  #          'ranger', 'tidyverse', 'reshape2',
  #          'hrbrthemes', 'ggpubr', 'Boruta')
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(caret, doParallel, randomForest, Boruta, stringr, dplyr))

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/3a_Outliers.R'))
  source(paste0(r.dir,'/functions/3b_Boxplot.R'))
  source(paste0(r.dir,'/functions/3c_RFE.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[[1]]

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.variables.rds = paste0(proyecto.directorio,'/exploratorio/',BaseDatos,'/rds/',str_replace(VarObj,'[.]','-'))
  dir.create(exploratorio.variables.rds, recursive = T, mode = "0777", showWarnings = F)
  exploratorio.variables.figuras = paste0(proyecto.directorio,'/exploratorio/',BaseDatos,'/figuras/',str_replace(VarObj,'[.]','-'))
  dir.create(exploratorio.variables.figuras, recursive = T, mode = "0777", showWarnings = F)
  modelos.particion.datos = paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.particion.datos, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  datos.entrada <- paste0(proyecto.directorio,'/datos/salida')

  # Cargar matrix observaciones
  matriz <- read.csv(paste0(datos.entrada,'/0_matriz/MatrixDatos.csv'),sep=',')
  if (BaseDatos != 'AMBAS'){
    matriz = matriz[which(matriz$TIPO == BaseDatos),]
  }

  covariables <- readRDS(paste0(datos.entrada,'/1_covariables/covariables.rds'))

  final_df <- data.frame(matriz[,c('COD_PERFIL','LATITUD','LONGITUD')], target=matriz[,VarObj], matriz[,which(names(matriz) %in% covariables)])
  print(dim(final_df))
  
  # identificar y remote outliers
  gooddata = computeOutliers(matriz[,covariables], type = 'remove')
  good_df_q95 = final_df[gooddata,]

  df_wnoise = good_df_q95

  # Remover variables con cero variabilidad
  df_wnoise[,nearZeroVar(df_wnoise)] = NULL

  # Datos finales
  data <- df_wnoise

  # Remover NAs - ##TODO eliminar variables con muchos NAs o eliminar registros
  data <- data[complete.cases(data), ]
  
  write.csv(table(data$target), 'F:/IDI_MDS_Agrologia/script/soil-toolbox/proyecto_cesarmagdalena/soporte/grangrupo.csv', row.names = FALSE)
  
  if (is(data$target,'character')){
    # si la variable es categorica solo dejar clases con al menos de 5 observaciones
    # As a rule of thumb, a class to be modelled should have at least 5 observations
    # source: https://soilmapper.org/soilmapping-using-mla.html
    original <- unique(data$target)
    data <- data[data$target %in%  names(table(data$target))[table(data$target) >= 5] , ]
    remove_all_ws<- function(string){
      return(gsub(" ", "_", str_squish(string)))
    }
    data <- data %>%
      mutate_if(is.character, remove_all_ws)
    data$target <- factor(data$target)
    final <- unique(data$target)
    print(paste0(original[!(original %in% final)], collapse=', '))
    METRIC <- 'Accuracy'
  } else {
    METRIC <- 'RMSE'
  }

  data_model <-data.frame(target=data[,'target'], data[,which(names(data) %in% covariables)])
  data_info <-data[,c('COD_PERFIL','LATITUD','LONGITUD')]
  print(dim(data_model))
  
  ##Conjunto de datos para entrenamiento y para validacion
  set.seed(225)
  inTrain <- createDataPartition(y = data_model[,1], p = .70, list = FALSE)
  train_data <- as.data.frame(data_model[inTrain,])
  test_data <- as.data.frame(data_model[-inTrain,])
  particion <- list(train=train_data,test=test_data)
  save(particion, file=paste0(modelos.particion.datos,'/particion.RData'))
  write.csv(train_data, file=paste0(modelos.particion.datos,'/entrenamiento.csv'), row.names=FALSE)
  write.csv(test_data, file=paste0(modelos.particion.datos,'/evaluacion.csv'), row.names=FALSE)
  
  ##Exportar datos info coordenadas
  data_info[inTrain,'particion'] <- 'entrenamiento'
  data_info[-inTrain,'particion'] <- 'evaluacion'
  write.csv(data_info, file=paste0(modelos.particion.datos,'/coordenadas.csv'), row.names=FALSE)
  print(dim(train_data))
  print(dim(test_data))
  
  #Definir muestras de entrenamiento
  data <- train_data
  ##Seleccion de variables --> RFE
  file_name <- 'rfe.rds'
  exploratorio.variables.rfe <- paste0(exploratorio.variables.rds,'/',file_name)
  if (!file.exists(exploratorio.variables.rfe)){
    cat(paste0('Ejecutando la seleccion de variables de la variable objetivo ',VarObj,' usando el algoritmo RFE'),'\n','\n')
    #set.seed(40)
    search_limit <- round(dim(data[,-1])[2]/2)
    search_diff <- round((dim(data[,-1])[2]-search_limit)/3)
    search_regular <- seq(search_limit, dim(data[,-1])[2], search_diff)
    if (search_regular[length(search_regular)] == dim(data[,-1])[2]){
      subset = c(1:search_limit,search_regular[-1])
    } else{
      subset = c(1:search_limit,search_regular[-1], dim(data[,-1])[2])
    }
    #para que sea reproducible (fuente: https://stackoverflow.com/questions/32290513/making-recursive-feature-elimination-using-caret-parallel-on-windows)
    seeds <- vector(mode = "list", length = 51)
    for(i in 1:50) seeds[[i]] <- sample.int(1000, length(subset) + 1)
    seeds[[51]] <- sample.int(1000, 1)
    
    #procesamiento en paralelo
    start <- Sys.time()
    no_cores <- detectCores() - 1
    cl <- makeCluster(no_cores, type = "SOCK")
    registerDoParallel(cl)
    control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5, seeds = seeds) #number=10, repeats=10 de acuerdo FAO sin embargo MGuevara usa 5 https://github.com/DSM-LAC/MEXICO/search?q=rfe
    (rfmodel <- rfe(x=data[,-1], y=data[,1], sizes=subset, rfeControl=control2)) #sizes se refiere al detalle de la curva,
    stopCluster(cl = cl)
    print(Sys.time() - start)
    
    #exportar imagen
    png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 550)
    print(plot(rfmodel, type=c("g", "o"), cex=2,cex.names = 2, metric = METRIC))
    dev.off()
    save(rfmodel, file=exploratorio.variables.rfe)
    
  } else {
    cat(paste0('El archivo RDS y figura de la selección de variables con el método RFE de la variable objetivo ',VarObj,' ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.rfe)),'\n'))
  }

  file_name <- 'boruta.rds'
  exploratorio.variables.boruta <- paste0(exploratorio.variables.rds,'/',file_name)
  if (!file.exists(exploratorio.variables.boruta)){
    cat(paste0('Ejecutando la selección de variables de la variable objetivo ',VarObj,' usando el algoritmo Boruta'),'\n','\n')
    nCores <- detectCores() - 1
    start <- Sys.time()
    set.seed(123)
    formula <- as.formula('target ~ .')
    (bor <- Boruta(formula, data = data, doTrace = 0, num.threads = nCores, ntree = 30, maxRuns=500)) #se debe evaluar ntree (numero de arboles), maxRuns (cantidad de interacciones)
    save(bor, file=exploratorio.variables.boruta)
    png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 550)
    par(mar = c(18, 4, 1, 1))
    plot(bor, cex.axis=1.3, las=2, xlab="", cex=0.75)
    dev.off()
    print(Sys.time() - start)
  } else {
    cat(paste0('El archivo RDS y figura de la selección de variables con el método Boruta de la variable objetivo ',VarObj,' ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.boruta)),'\n','\n'))
  }

  if (is(data$target,'factor')){
    #Definir muestras de entrenamiento
    set.seed(123)
    down_train <- downSample(x = train_data[, -ncol(train_data)],
                         y = train_data$target)
    down_train$Class <- NULL

    data <- down_train
    ##Seleccion de variables --> RFE
    file_name <- 'rfe_down.rds'
    exploratorio.variables.rfe <- paste0(exploratorio.variables.rds,'/',file_name)
    if (!file.exists(exploratorio.variables.rfe)){
      cat(paste0('Ejecutando la selección de variables de la variable objetivo ',VarObj,' usando el algoritmo RFE con el dataset BALANCEADO'),'\n','\n')
      start <- Sys.time()
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")
      registerDoParallel(cl)
      set.seed(40)
      control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5) #number=10, repeats=10 de acuerdo FAO sin embargo MGuevara usa 5 https://github.com/DSM-LAC/MEXICO/search?q=rfe
      search_limit <- round(dim(data[,-1])[2]/2)
      search_diff <- round((dim(data[,-1])[2]-search_limit)/3)
      search_regular <- seq(search_limit, dim(data[,-1])[2], search_diff)
      if (search_regular[length(search_regular)] == dim(data[,-1])[2]){
        subset = c(1:search_limit,search_regular[-1])
      } else{
        subset = c(1:search_limit,search_regular[-1], dim(data[,-1])[2])
      }
      print(subset)
      (rfmodel <- rfe(x=data[,-1], y=data[,1], sizes=subset, rfeControl=control2)) #sizes se refiere al detalle de la curva,
      stopCluster(cl = cl)
      png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 550)
      print(plot(rfmodel, type=c("g", "o"), cex=2,cex.names = 2, metric = METRIC))
      dev.off()
      predictors(rfmodel)[1:10]
      save(rfmodel, file=exploratorio.variables.rfe)
      print(Sys.time() - start)
    } else {
      cat(paste0('El archivo RDS y figura de la selección de variables con el método RFE de la variable objetivo ',VarObj,' con el dataset BALANCEADO ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.rfe)),'\n'))
    }

    file_name <- 'boruta_down.rds'
    exploratorio.variables.boruta <- paste0(exploratorio.variables.rds,'/',file_name)
    if (!file.exists(exploratorio.variables.boruta)){
      cat(paste0('Ejecutando la selección de variables de la variable objetivo ',VarObj,' con el dataset BALANCEADO usando el algoritmo Boruta'),'\n','\n')
      nCores <- detectCores() - 1
      start <- Sys.time()
      formula <- as.formula('target ~ .')
      (bor <- Boruta(formula, data = data, doTrace = 0, num.threads = nCores, ntree = 30, maxRuns=500)) #se debe evaluar ntree (numero de arboles), maxRuns (cantidad de interacciones)
      save(bor, file=exploratorio.variables.boruta)
      png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 550)
      par(mar = c(18, 4, 1, 1))
      plot(bor, cex.axis=1.3, las=2, xlab="", cex=0.75)
      dev.off()
      print(Sys.time() - start)
    } else {
      cat(paste0('El archivo RDS y figura de la selección de variables con el método Boruta de la variable objetivo ',VarObj,'  con el dataset BALANCEADO ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.boruta)),'\n','\n'))
    }
  }
}