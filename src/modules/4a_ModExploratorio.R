#############################################################################
# titulo        : Exploración de los datos de entrada;
# proposito     : Explorar los datos de entrada, entrenamiento y evaluación;
# autor(es)     : Preparado por Andrés Lopez (AL) y Patricia Escudero (PE), IGAC-CIAF; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado ACC en Bogotá, Colombia;
# entrada       : Particición Datos de Entrenamiento y Evaluación;
# salida        : Gráficas indicando relación datos con la variable objetivo;
# observaciones : ninguna;
##############################################################################

ModExploracion <- function(VarObj, rfe_lim){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg <- c('ggplot2', 'tidyr', 'PerformanceAnalytics')

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

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/rds/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/0_particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.entrada, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/1_exploratorio/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(modelos.salida, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Cargar particición
  load(paste0(modelos.entrada,'/particion.RData'))
  train.data <- as.data.frame(particion['train'])
  names(train.data) <- sub('train.', "", names(train.data))
  train.data$particion = 'train'
  test.data <- as.data.frame(particion['test'])
  names(test.data) <- sub('test.', "", names(test.data))
  test.data$particion = 'test'

  data = rbind(train.data,test.data)

  # Cargar rfe
  load(paste0(exploratorio.variables,'/rfe.rds'))

  # ------------------------------------------------------- #
  # Definir variables globales
  # ------------------------------------------------------- #
  global_clima <- c('Cálido, húmedo','Cálido, muy seco','Cálido, seco','Frío, húmedo','Subnival, Pluvial','Templado, húmedo')
  names(global_clima) <- c(1, 2, 3, 7, 13, 14)

  global_relieve <- c('Terraza aluvial nivel 1','Colina','Glacis de acumulación','Loma',
                         'Valle estrecho','Depresión','Vallecito','Plano de inundación de rio meóndrico activo',
                      'Plano de inundación de rio meóndrico inactivo',
                      'Fila y viga','Abanico aluvial antiguo','Abanico aluvial reciente','Abanico aluvial subreciente',
                      'Cumbre','Cono de deyección')

  names(global_relieve) <- c(1:9, 14, 15, 16, 17, 20, 21)

  # ------------------------------------------------------- #
  # Graficar según tipo de variable
  # ------------------------------------------------------- #
  if (is(train.data[,'target'],'numeric')){

    # covariables
    covars = predictors(rfmodel)[1:rfe_lim]

    # directorios
    carpeta.correlacion = paste0(modelos.salida,'/correlacion')
    dir.create(carpeta.correlacion, recursive = T, mode = "0777", showWarnings = F)
    carpeta.individuales.clima = paste0(modelos.salida,'/individuales/clima')
    dir.create(carpeta.individuales.clima, recursive = T, mode = "0777", showWarnings = F)
    carpeta.individuales.relieve = paste0(modelos.salida,'/individuales/relieve')
    dir.create(carpeta.individuales.relieve, recursive = T, mode = "0777", showWarnings = F)
    carpeta.individuales.climaxrelieve = paste0(modelos.salida,'/individuales/climaxrelieve')
    dir.create(carpeta.individuales.climaxrelieve, recursive = T, mode = "0777", showWarnings = F)

    # lista de graficos
    lista.graficos = c('train_correlationmatrix','test_correlationmatrix', sprintf('%s_clima', covars), sprintf('%s_relieve', covars),sprintf('%s_climaxrelieve', covars))
    lista.graficos.procesados = gsub("\\.png$", "", basename(list.files(path= modelos.salida, pattern = "\\.png$", recursive = T)))

    lista.graficos.faltantes <- setdiff(lista.graficos,lista.graficos.procesados)

    if (length(lista.graficos.faltantes) > 0){
      # Graficos matrix de correlación
      for (j in lista.graficos.faltantes){
        if (j == 'train_correlationmatrix'){
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == 'train', covars])
          par(xpd=TRUE)
          dev.off()
        }
        else if (j == 'test_correlationmatrix'){
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == 'test', covars])
          par(xpd=TRUE)
          dev.off()
        }
        else if (endsWith(j, '_clima')){
          ## Graficos comparaciones
          clima.clases = factor(data[data$particion == 'train', 'clima'])
          clima.clases.grafica = as.vector(global_clima[which(names(global_clima) %in% clima.clases)])

          i = strsplit(j, "_")[[1]][1]

          png(file = paste0(carpeta.individuales.clima,'/',j,'.png'), width = 700, height = 400)
          p <- ggplot(data=data[data$particion == 'train', names(data)], aes_string(x=i, y='target', colour=sprintf("factor(%s)","clima"))) +
            geom_point(alpha = 0.6) +
            scale_color_discrete(name='Clima',labels=clima.clases.grafica) +
            xlab(i) +
            ylab(VarObj) +
            theme(legend.position='top') +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
            theme_light()
            print(p)
          dev.off()
        }
        else if (endsWith(j, '_relieve')){
          ## Graficos comparaciones
          relieve.clases = factor(data[data$particion == 'train', 'tipo_relieve'])
          relieve.clases.grafica = as.vector(global_relieve[which(names(global_relieve) %in% relieve.clases)])

          i = strsplit(j, "_")[[1]][1]

          png(file = paste0(carpeta.individuales.relieve,'/',j,'.png'), width = 700, height = 400)
          p <- ggplot(data=data[data$particion == 'train', names(data)], aes_string(x=i, y='target', colour=sprintf("factor(%s)","tipo_relieve"))) +
            geom_point(alpha = 0.6) +
            scale_color_discrete(name='Relieve', labels=relieve.clases.grafica) +
            xlab(i) +
            ylab(VarObj) +
            theme(legend.position='top') +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
            theme_light()
            print(p)
          dev.off()
        }
        else if (endsWith(j, '_climaxrelieve')){
          ## Graficos comparaciones
          clima.clases = factor(data[data$particion == 'train', 'clima'])
          clima.clases.grafica = as.vector(global_clima[which(names(global_clima) %in% clima.clases)])

          relieve.clases = factor(data[data$particion == 'train', 'tipo_relieve'])
          relieve.clases.grafica = as.vector(global_relieve[which(names(global_relieve) %in% relieve.clases)])

          i = strsplit(j, "_")[[1]][1]

          png(file = paste0(carpeta.individuales.climaxrelieve,'/',j,'.png'), width = 700, height = 400)
          p <- ggplot(data=data[data$particion == 'train', names(data)], aes_string(x=i, y='target', colour=sprintf("factor(%s)","tipo_relieve"), shape = sprintf("factor(%s)","clima"))) +
            geom_point(alpha = 0.6) +
            scale_shape_discrete(name='Clima', labels=clima.clases.grafica) +
            scale_color_discrete(name='Relieve', labels=relieve.clases.grafica) +
            xlab(i) +
            ylab(VarObj) +
            theme(legend.position='top') +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
            theme_light()
            print(p)
          dev.off()
        }
      }


      }
  }
  else if (is(train.data[,'target'],'factor')){
      # As a rule of thumb, a class to be modelled should have at least 5 observations
      # source: https://soilmapper.org/soilmapping-using-mla.html
  }
}