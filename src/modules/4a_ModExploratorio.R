#############################################################################
# titulo        : Exploracion de los datos de entrada;
# proposito     : Explorar los datos de entrada, entrenamiento y evaluacion;
# autor(es)     : Preparado por Andres Lopez (AL) y Patricia Escudero (PE), IGAC-CIAF; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado ACC en Bogota�, Colombia;
# entrada       : Particicion Datos de Entrenamiento y Evaluacion;
# salida        : Graficas indicando relacion datos con la variable objetivo;
# observaciones : ninguna;
##############################################################################

prompt.user.part4a <- function()#get arguments from user
{
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  variables.usuario <- VariablesObjectivo()
  cat(paste0('Las siguientes columnas estan disponibles para su modelación:','\n'))
  cat(paste0(variables.usuario, sep=" | "))
  cat(paste0('\n','\n'))

  message(prompt="Indique el nombre de la variable objetivo de acuerdo al listado superior:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el tipo de base de datos para modelar (AMBAS, PERFIL, OBSERVACION):>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  message(prompt="Indique el numero limite de covariables a considerar según interpretación del RFE y Boruta:>>> ")
  c <- readLines(n = 1)
  c <- gsub("\\\\", "/", c)

  newlist = list(a, b, c)
}


ModExploracion <- function(VarObj, BaseDatos, rfe_lim){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(ggplot2, tidyr, PerformanceAnalytics, stringr, caret,
                                  pals, psych))
  
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/5_Predict.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[[1]]
  project.name <- sapply(strsplit(proyecto.directorio, '_'), tail, 1)

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/',BaseDatos,'/rds/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.entrada, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/1_exploratorio/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  dir.create(modelos.salida, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  start_time <- Sys.time()

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

    covars = gsub('_','-',covars)
    
    # directorios
    carpeta.correlacion = paste0(modelos.salida,'/correlacion')
    dir.create(carpeta.correlacion, recursive = T, mode = "0777", showWarnings = F)
    carpeta.dispersion.clima = paste0(modelos.salida,'/dispersion/clima')
    dir.create(carpeta.dispersion.clima, recursive = T, mode = "0777", showWarnings = F)
    carpeta.dispersion.relieve = paste0(modelos.salida,'/dispersion/relieve')
    dir.create(carpeta.dispersion.relieve, recursive = T, mode = "0777", showWarnings = F)
    carpeta.boxplot.clima = paste0(modelos.salida,'/boxplot')
    dir.create(carpeta.boxplot.clima, recursive = T, mode = "0777", showWarnings = F)

    # lista de graficos
    lista.graficos.dispersion = c(sprintf('%s_dispersion_clima', covars), sprintf('%s_dispersion_relieve', covars))
    lista.graficos = c('train_correlationmatrix','test_correlationmatrix', lista.graficos.dispersion, sprintf('clima_boxplot_%s', VarObj))
    lista.graficos.procesados = gsub("\\.png$", "", basename(list.files(path= modelos.salida, pattern = "\\.png$", recursive = T)))

    lista.graficos.faltantes <- setdiff(lista.graficos,lista.graficos.procesados)

    if (length(lista.graficos.faltantes) > 0){
      # Graficos matrix de correlación
      for (j in lista.graficos.faltantes){
        if (j == 'train_correlationmatrix'){
          covars = gsub('-','_',covars)
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == 'train', covars])
          par(xpd=TRUE)
          dev.off()
        }
        else if (j == 'test_correlationmatrix'){
          covars = gsub('-','_',covars)
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == 'test', covars])
          par(xpd=TRUE)
          dev.off()
        }
        else if (endsWith(j, 'dispersion_clima')){
          ## Graficos comparaciones
          clima.clases = factor(data[data$particion == 'train', 'clima'])
          clima.clases.grafica = as.vector(global_clima[which(names(global_clima) %in% clima.clases)])

          i = strsplit(j, "_")[[1]][1]
          i = gsub('-','_',i)

          png(file = paste0(carpeta.dispersion.clima,'/',j,'.png'), width = 700, height = 400)
          p <- ggplot(data=data[data$particion == 'train', names(data)], aes_string(x=i, y='target', colour=sprintf("factor(%s)","clima"))) +
            geom_point(alpha = 0.4) +
            scale_color_discrete(name='Clima',labels=clima.clases.grafica) +
            xlab(i) +
            ylab(VarObj) +
            theme(legend.position='top') +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
            theme_bw()
            print(p)
          dev.off()
        }
        else if (endsWith(j, 'dispersion_relieve')){
          ## Graficos comparaciones
          relieve.clases = factor(data[data$particion == 'train', 'relieve'])
          relieve.clases.grafica = as.vector(global_relieve[which(names(global_relieve) %in% relieve.clases)])

          i = strsplit(j, "_")[[1]][1]
          i = gsub('-','_',i)

          png(file = paste0(carpeta.dispersion.relieve,'/',j,'.png'), width = 700, height = 400)
          p <- ggplot(data=data[data$particion == 'train', names(data)], aes_string(x=i, y='target', colour=sprintf("factor(%s)","relieve"))) +
            geom_point(alpha = 0.4) +
            scale_color_discrete(name='Relieve', labels=relieve.clases.grafica) +
            xlab(i) +
            ylab(VarObj) +
            theme(legend.position='top') +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
            theme_bw()
            print(p)
          dev.off()
        }
        else if (j == sprintf('clima_boxplot_%s', VarObj)){
          ## Graficos comparaciones
          clima.clases = factor(data[data$particion == 'train', 'clima'])
          clima.clases.grafica = as.vector(global_clima[which(names(global_clima) %in% clima.clases)])

          i = strsplit(j, "_")[[1]][1]
          i = gsub('-','_',i)

          png(file = paste0(carpeta.boxplot.clima,'/',j,'.png'), width = 700, height = 400)
          p <- ggplot(data = data[data$particion == 'train', names(data)], aes_string(x=sprintf("factor(%s)",i), y='target', fill=sprintf("factor(%s)",i))) +
            geom_boxplot() +
            ylab(VarObj) +
            scale_fill_discrete(name='Clima', labels=clima.clases.grafica) +
            theme(axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
            theme(legend.position='top') +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
            theme_bw()
            print(p)
          dev.off()
        }
      }


      }
  }
  else if (is(train.data[,'target'],'factor')){
      # As a rule of thumb, a class to be modelled should have at least 5 observations
      # source: https://soilmapper.org/soilmapping-using-mla.html
    # covariables
    covars = predictors(rfmodel)[1:rfe_lim]
    covars = gsub('_','-',covars)
    print(paste0(covars, collapse=', '))
    
    # directorios
    carpeta.correlacion = paste0(modelos.salida,'/correlacion')
    dir.create(carpeta.correlacion, recursive = T, mode = "0777", showWarnings = F)

    # lista de graficos
    lista.graficos = c('train_correlationmatrix','test_correlationmatrix')
    lista.graficos.procesados = gsub("\\.png$", "", basename(list.files(path= modelos.salida, pattern = "\\.png$", recursive = T)))
    
    lista.graficos.faltantes <- setdiff(lista.graficos,lista.graficos.procesados)
    
    if (length(lista.graficos.faltantes) > 0){
      # Graficos matrix de correlación
      for (j in lista.graficos.faltantes){
        if (j == 'train_correlationmatrix'){
          
          covars = gsub('-','_',covars)
          
          n<-length(levels(as.factor(data[data$particion == 'train', 'target'])))
          
          cols <- pals::cols25(n)
          
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == 'train', covars])
          #pairs.panels(data[data$particion == 'train', covars],bg=cols[data[data$particion == 'train', 'target']],
          #             pch=21,main="Covariables por Grupo",hist.col="blue")
          
          #chart.Correlation(data[data$particion == 'train', covars], bg=as.factor(data[data$particion == 'train', 'target']), pch=3)
          #par(xpd=TRUE)
          #legend(0, 1, as.vector(unique(data[data$particion == 'train', 'target'])), fill=seq(1:length(unique(data[data$particion == 'train', 'target']))))
          dev.off()
        }
        else if (j == 'test_correlationmatrix'){
          covars = gsub('-','_',covars)
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == 'test', covars])
          dev.off()
        }
      }
    }
  }

  #estimar tiempo de procesamiento total
  print(Sys.time() - start_time)

}