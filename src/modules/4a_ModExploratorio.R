#############################################################################
# titulo        : Exploracion de los datos de entrada;
# proposito     : Explorar los datos de entrada, entrenamiento y evaluacion;
# autor(es)     : Preparado por Andres Lopez (AL) y Patricia Escudero (PE), IGAC-CIAF; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado ACC en Bogota, Colombia;
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
  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(ggplot2, tidyr, PerformanceAnalytics, stringr, caret,
                                  pals, psych, purrr, nortest, tseries, rstatix, PMCMR,
                                  rcompanion, multcompView))
  
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
  proyecto.directorio <- conf.args[['proyecto.carpeta']]
  project.covars.vector <- conf.args[['covariables.vector']]
  project.covars.vector <- unlist(strsplit(project.covars.vector,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  metadatos.categoricas <- paste0(proyecto.directorio,'/datos/entrada/1_covariables/raster')
  exploratorio.variables <- paste0(proyecto.directorio,'/exploratorio/',BaseDatos,'/rds/',str_replace(VarObj,'[.]','-'))
  modelos.entrada <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/0_particion/',str_replace(VarObj,'[.]','-'))
  dir.create(modelos.entrada, recursive = T, mode = "0777", showWarnings = F)
  modelos.salida <- paste0(proyecto.directorio,'/modelos/',BaseDatos,'/1_exploratorio/',str_replace(VarObj,'[.]','-'),'/',rfe_lim,'_covariables')
  carpeta.descriptiva = paste0(modelos.salida,'/1_descriptivo')
  dir.create(carpeta.descriptiva, recursive = T, mode = "0777", showWarnings = F)
  carpeta.graficos = paste0(modelos.salida,'/2_graficos')
  dir.create(carpeta.graficos, recursive = T, mode = "0777", showWarnings = F)
  carpeta.estadistico = paste0(modelos.salida,'/3_estadistico')
  dir.create(carpeta.estadistico, recursive = T, mode = "0777", showWarnings = F)

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
  # Graficar según tipo de variable
  # ------------------------------------------------------- #
  if (is(train.data[,'target'],'numeric')){

    # covariables
    covars = predictors(rfmodel)[1:rfe_lim]
    covars = gsub('_','-',covars)
    covars_categoricas = project.covars.vector[project.covars.vector %in% covars]
    covars_continuas = covars[!covars %in% project.covars.vector]

    # directorios correlaciones
    carpeta.correlacion = paste0(carpeta.graficos,'/correlacion')
    dir.create(carpeta.correlacion, recursive = T, mode = "0777", showWarnings = F)

    # descriptivo variable objetivo
    desc_target <- describe(data[data$particion == 'train', 'target'])
    desc_target$vars <- VarObj
    write.csv(desc_target,paste0(carpeta.descriptiva,'/1_descriptivo_variableobjetivo.csv'),row.names = FALSE)

    # descriptivo covariables continuas
    desc_continuas <- describe(data[data$particion == 'train', covars_continuas])
    desc_continuas$vars <- covars_continuas
    write.csv(desc_continuas,paste0(carpeta.descriptiva,'/2_descriptivo_covariables-continuas.csv'),row.names = FALSE)

    #crear directorios y cargar metadatados si hay covariables categoricas
    if (length(covars_categoricas) > 0){
      #crear directorios
      carpeta.boxplot = paste0(carpeta.graficos,'/boxplot')
      dir.create(carpeta.boxplot, recursive = T, mode = "0777", showWarnings = F)

      metadata_categoricas = list()
      for (i in covars_categoricas){
        #directorios
        carpeta.dispersion = paste0(carpeta.graficos,'/dispersion/',i)
        dir.create(carpeta.dispersion, recursive = T, mode = "0777", showWarnings = F)
        #metadatos
        metadata_categoricas[[i]] <- read.csv(paste0(metadatos.categoricas,'/',i,'/',i,'.csv'))
      }

      # descriptivo covariables categoricas
      DescCategoricas <- function(covar){
        clases = factor(data[data$particion == 'train', covar])
        global_metadatos <- metadata_categoricas[[covar]]
        clases.tabla = as.vector(global_metadatos[global_metadatos$ID %in% clases, 'GRUPO'])

        freq <- table(data[data$particion == 'train', covar])
        out_tb <- data.frame(covariable=covar, clases=clases.tabla, Frecuencia=freq)
        return(out_tb)
      }

      desc_categoricas <- lapply(covars_categoricas, function(x){DescCategoricas(x)})
      names(desc_categoricas) <- covars_categoricas

      # Exportar tablas por separado
      lapply(names(desc_categoricas), function(x) write.csv(desc_categoricas[[x]],
        file =paste0(carpeta.descriptiva,'/3_descriptivo_covariable-categorica_',x,'.csv'), row.names=FALSE))

      # lista de graficos dispersion
      lista.graficos.dispersion = list()
      for (covar_cat in covars_categoricas){
        lista.temp <- sprintf(paste0('%s_',covar_cat,'_dispersion'), covars_continuas)
        lista.graficos.dispersion[[covar_cat]] <- lista.temp
      }
      lista.graficos.dispersion <- lista.graficos.dispersion %>% flatten_chr()

      # lista de graficos boxplots
      lista.graficos.boxplots = sprintf('%s_boxplot', covars_categoricas)

      lista.graficos = c('train_correlationmatrix','test_correlationmatrix', lista.graficos.dispersion, lista.graficos.boxplots)
    } else{
      lista.graficos = c('train_correlationmatrix','test_correlationmatrix')
    }

    lista.graficos.procesados = gsub("\\.png$", "", basename(list.files(path= modelos.salida, pattern = "\\.png$", recursive = T)))
    lista.graficos.faltantes <- setdiff(lista.graficos,lista.graficos.procesados)

    if (length(lista.graficos.faltantes) > 0){
      for (j in lista.graficos.faltantes){
        if (endsWith(j, 'correlationmatrix')){
          particion <- sapply(strsplit(j, "_", fixed = TRUE), "[", 1)
          png(file = paste0(carpeta.correlacion,'/',j,'.png'), width = 1000, height = 700)
          chart.Correlation(data[data$particion == particion, covars_continuas])
          par(xpd=TRUE)
          dev.off()
        }
        else if (endsWith(j, 'dispersion')){
          tipo <- sapply(strsplit(j, "_", fixed = TRUE), "[", 2)
          tarVar <- sapply(strsplit(j, "_", fixed = TRUE), "[", 1)

          clases = factor(data[data$particion == 'train', tipo])
          global_metadatos <- metadata_categoricas[[tipo]]
          clases.grafica = as.vector(global_metadatos[global_metadatos$ID %in% clases, 'GRUPO'])

          fn <- paste0(tarVar,'_',tipo,'_dispersion.png')
          png(file = paste0(carpeta.graficos,'/dispersion/',tipo,'/',fn), width = 700, height = 400)
          p <- ggplot(data=data[data$particion == 'train', names(data)], aes_string(x=tarVar, y='target', colour=sprintf("factor(%s)",tipo))) +
            geom_point(alpha = 0.4) +
            scale_color_discrete(name=tipo,labels=clases.grafica) +
            xlab(tarVar) +
            ylab(VarObj) +
            theme(legend.position='top') +
            guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
            theme_bw()
            print(p)
          dev.off()

        }
        else if (endsWith(j, 'boxplot')){
          tipo <- sapply(strsplit(j, "_", fixed = TRUE), "[", 1)

          clases = factor(data[data$particion == 'train', tipo])
          global_metadatos <- metadata_categoricas[[tipo]]
          clases.grafica = as.vector(global_metadatos[global_metadatos$ID %in% clases, 'GRUPO'])

          png(file = paste0(carpeta.graficos,'/boxplot/',tipo,'_boxplot.png'), width = 700, height = 400)
          p <- ggplot(data = data[data$particion == 'train', names(data)], aes_string(x=sprintf("factor(%s)",tipo), y='target', fill=sprintf("factor(%s)",tipo))) +
            geom_boxplot() +
            ylab(VarObj) +
            scale_fill_discrete(name=tipo, labels=clases.grafica) +
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

    # Estadistico
    target_dataset = data[data$particion == 'train',]

    if (dim(target_dataset)[1] > 50){
      ## Pruebas normalidad
      # Test lillie
      lillie <- lillie.test(x=target_dataset$target)
      # Test Jarque-Bera
      jarque <- jarque.bera.test(x=target_dataset$target)
      # Crear dataframe
      normalidad <- data.frame(prueba=c('Lilliefors','Jarque Bera Test'),pvalue=c(lillie$p.value,jarque$p.value))
      # agregar significancia
      normalidad <- normalidad %>% mutate(Significancia = case_when(
                pvalue < 0.0001  ~ "**** <0,0001",
                pvalue < 0.001  ~ "*** <0,001",
                pvalue < 0.01   ~ "** <0,01",
                pvalue < 0.05   ~ "*<0,05",
                TRUE ~ "NS"
              ))

      ## Kruskal wallis
      KWdata <- function(covar){
        kw <- kruskal.test(target_dataset[,'target']~target_dataset[,covar])
        out_tb <- data.frame(covariable=covar, chi.squared=kw$statistic, pvalue=kw$p.value)
        return(out_tb)
      }

      KW.resultados <- lapply(covars, function(x){KWdata(x)})
      KW.resultados <- do.call('rbind', KW.resultados)
      # agregar significancia
      KW.resultados <- KW.resultados %>% mutate(Significancia = case_when(
                pvalue < 0.0001  ~ "**** <0,0001",
                pvalue < 0.001  ~ "*** <0,001",
                pvalue < 0.01   ~ "** <0,01",
                pvalue < 0.05   ~ "*<0,05",
                TRUE ~ "NS"
              ))

      ## Posthoc
      covar.signif <- KW.resultados[which(KW.resultados['pvalue']<0.05),'covariable']
      categoricas_posthoc = covar.signif[covar.signif %in% covars_categoricas]

      postHOCdata <- function(covar){
        clases = factor(data[data$particion == 'train', covar])
        global_metadatos <- metadata_categoricas[[covar]]
        clases = as.vector(global_metadatos[global_metadatos$ID %in% clases, 'GRUPO'])

        phoc <- posthoc.kruskal.nemenyi.test(target_dataset[,'target']~target_dataset[,covar],p.adjust.method='Bonferroni')

        # Cambiar formarto y asignar nombres
        PT <- phoc$p.value
        PT1 <- data.frame(fullPTable(PT))
        colnames(PT1) <- clases
        rownames(PT1) <- clases

        PT1_levels <- PT1 %>% mutate_at(vars(colnames(PT1)),  ~ case_when(. < 0.001 ~ "*** <0,001",
                                                             . < 0.01   ~ "** <0,01",
                                                             . < 0.05   ~ "* <0,05",
                                                             TRUE ~ "NS"))
        rownames(PT1_levels) <- clases

        output <- list(PT1,PT1_levels)

        return(output)
      }

      phoc.resultados <- lapply(categoricas_posthoc, function(x){postHOCdata(x)})
      names(phoc.resultados) <- categoricas_posthoc

      # Exportar resultados normalidad
      write.csv(normalidad,paste0(carpeta.estadistico,'/1_normalidad.csv'),row.names=FALSE)
      write.csv(KW.resultados,paste0(carpeta.estadistico,'/2_kruskal_grupos.csv'),row.names=FALSE)

      # Export posthoc tabla y boxplot
      lapply(names(phoc.resultados), function(x) write.csv(phoc.resultados[[x]][1],
        file =paste0(carpeta.estadistico,'/3a_posthoc-valores_',x,'.csv'), row.names=TRUE))

      lapply(names(phoc.resultados), function(x) write.csv(phoc.resultados[[x]][2],
        file =paste0(carpeta.estadistico,'/3b_posthoc-significancia_',x,'.csv'), row.names=TRUE))
    }
  }
  else if (is(train.data[,'target'],'factor')){
    # covariables
    covars = predictors(rfmodel)[1:rfe_lim]
    covars = gsub('_','-',covars)

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
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),1))

}