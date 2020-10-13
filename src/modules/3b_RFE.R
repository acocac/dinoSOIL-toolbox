#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ExpRFE <- function(VarObj){
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
  pckg <- c('caret', 'doParallel', 'randomForest', 'Boruta', 'stringr')

  usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }

  lapply(pckg,usePackage)

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
  exploratorio.variables.rds = paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  dir.create(exploratorio.variables.rds, recursive = T, mode = "0777", showWarnings = F)
  exploratorio.variables.figuras = paste0(proyecto.directorio,'/exploratorio/figuras/',str_replace(VarObj,'[.]','-'))
  dir.create(exploratorio.variables.figuras, recursive = T, mode = "0777", showWarnings = F)
  modelos.particion.datos = paste0(proyecto.directorio,'/modelos/0_particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.particion.datos, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  datos.entrada <- paste0(proyecto.directorio,'/datos/salida/1_covariables')

  # Cargar matrix observaciones
  matriz <- read.csv(paste0(datos.entrada,'/tabular/MatrixRegresion.csv'),sep=';')

  if (is(matriz[,VarObj],'numeric')){

    explanatory_exclude <- c(1:16,24,40,41) #algunas variables con artifactos (distribución espacial) son eliminadas y no tenidas en cuenta en RFE

    final_df <- data.frame(target=matriz[,VarObj], matriz[,-c(explanatory_exclude)])

    # identificar y remote outliers
    gooddata = computeOutliers(matriz[,-c(explanatory_exclude)], type = 'remove')
    good_df_q95 = final_df[gooddata,]

    df_wnoise = good_df_q95

    # Remover variables con cero variabilidad
    df_wnoise[,nearZeroVar(df_wnoise)] = NULL

    # Datos finales
    data <- df_wnoise

    # Remover NAs - ##TODO eliminar variables con muchos NAs o eliminar registros
    data <- data[complete.cases(data), ]

    ##Conjunto de datos para entrenamiento y para validacion
    set.seed(225)
    inTrain <- createDataPartition(y = data[,1], p = .70, list = FALSE)
    train_data <- as.data.frame(data[inTrain,])
    test_data <- as.data.frame(data[-inTrain,])
    particion <- list(train=train_data,test=test_data)
    save(particion, file=paste0(modelos.particion.datos,'/particion.RData'))

    ##Seleccion de variables --> RFE
    file_name <- 'rfe.rds'
    exploratorio.variables.rfe <- paste0(exploratorio.variables.rds,'/',file_name)
    if (!file.exists(exploratorio.variables.rfe)){
      cat('Ejecutando la selección de variables de la variable objetivo ',VarObj,' usando el algoritmo RFE','\n','\n')
      start <- Sys.time()
      no_cores <- detectCores() - 1
      cl <- makeCluster(no_cores, type = "SOCK")
      registerDoParallel(cl)
      set.seed(40)
      control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", number=5, repeats=5) #number=10, repeats=10 de acuerdo FAO
      (rfmodel <- rfe(x=data[,-1], y=data[,1], sizes=c(1:10), rfeControl=control2)) #sizes se refiere al detalle de la curva,
      stopCluster(cl = cl)
      png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 600)
      print(plot(rfmodel, type=c("g", "o")))
      dev.off()
      predictors(rfmodel)[1:10]
      save(rfmodel, file=exploratorio.variables.rfe)
      print(Sys.time() - start)
    } else {
      cat(paste0('El archivo RDS y figura de la selección de variables con el método RFE de la variable objetivo ',VarObj,' ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.rfe)),'\n'))
    }

    file_name <- 'boruta.rds'
    exploratorio.variables.boruta <- paste0(exploratorio.variables.rds,'/',file_name)
    if (!file.exists(exploratorio.variables.boruta)){
      cat('Ejecutando la selección de variables de la variable objetivo ',VarObj,' usando el algoritmo Boruta','\n','\n')
      nCores <- detectCores() - 1
      start <- Sys.time()
      formula <- as.formula('target ~ .')
      (bor <- Boruta(formula, data = data, doTrace = 0, num.threads = nCores, ntree = 30, maxRuns=500)) #se debe evaluar ntree (numero de arboles), maxRuns (cantidad de interacciones)
      save(bor, file=exploratorio.variables.boruta)
      png(file = paste0(exploratorio.variables.figuras,'/',str_replace(file_name,'.rds','.png')), width = 700, height = 600)
      plot(bor, xlab = "", xaxt = "n")
      lz<-lapply(1:ncol(bor$ImpHistory),function(i)
        bor$ImpHistory[is.finite(bor$ImpHistory[,i]),i])
      names(lz) <- colnames(bor$ImpHistory)
      Labels <- sort(sapply(lz,median))
      axis(side = 1,las=2,labels = names(Labels),
           at = 1:ncol(bor$ImpHistory), cex.axis = 0.7)
      dev.off()
      print(Sys.time() - start)
    } else {
      cat(paste0('El archivo RDS y figura de la selección de variables con el método Boruta de la variable objetivo ',VarObj,' ya existe y se encuentra en la ruta ',dirname(dirname(exploratorio.variables.boruta)),'\n','\n'))
    }

  }
}