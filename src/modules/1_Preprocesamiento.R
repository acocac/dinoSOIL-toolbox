#############################################################################
# titulo        : Verticalizacion bases de datos de suelo;
# proposito     : Verticalizar bases de datos de suelo provistas en formato horizontal;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# creacion      : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

prompt.user.part1 <- function()#get arguments from user
{

  message(prompt="Indique tipo de base de datos para verticalizar (PERFILES o OBSERVACIONES):>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el nombre del archivo EXCEL que contiene la base de datos horizontal:>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  message(prompt="Indique el nombre de la pestana del archivo EXCEL que contiene la base de datos:>>> ")
  c <- readLines(n = 1)
  c <- gsub("\\\\", "/", c)

  message(prompt="Indique el nombre de la columna que tiene el ID de los perfiles el archivo EXCEL que contiene la base de datos:>>> ")
  d <- readLines(n = 1)
  d <- gsub("\\\\", "/", d)

  newlist = list(a,b,c,d)
  return(newlist)
}

Preprocesamiento <- function(tipo, filename, hoja, columna){
  if (missing(filename))
    stop("Necesita especificar la base de datos para verticalizar.")

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(readxl, tidyr, dplyr))

  # iniciar el monitoreo tiempo de procesamiento total
  timeStart <- Sys.time()

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  project.folder <- conf.args[['proyecto.carpeta']]
  project.name <- sapply(strsplit(project.folder, '_'), tail, 1)
  project.vars.continuas <- conf.args[['vars.continuas']]
  project.vars.continuas <- unlist(strsplit(project.vars.continuas,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  datos.entrada.original <- paste0(project.folder,'/datos/entrada/0_basededatos/originales')
  datos.salida.procesado <- paste0(project.folder,'/datos/entrada/0_basededatos/derivados/verticalizado')
  dir.create(datos.salida.procesado, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(project.folder))

  # Prefijos archivos
  if (tipo == 'PERFILES'){
      file_prefix <- 'BDP'
  } else if (tipo == 'OBSERVACIONES'){
      file_prefix <- 'BDO'
  }

  verticalizada.archivo <- paste0(datos.salida.procesado,'/',file_prefix,'_',project.name,'_Vert.csv')
  if (!file.exists(verticalizada.archivo)){

    # ------------------------------------------------------- #
    # Carga y preparacion de los datos
    # ------------------------------------------------------- #
    # Base de datos (horizontal)
    datos_HOR <- data.frame(read_excel(paste0(datos.entrada.original,'/',filename), sheet = hoja, na = "N/A"))
    datos_HOR <- datos_HOR[!duplicated(datos_HOR[,columna]),]
    row.names(datos_HOR) <- datos_HOR[,columna]

    datos_HOR <- cbind(datos_HOR[,columna],datos_HOR[, grep('^H[0-9]', names(datos_HOR))])

    #detectar nombres con mas de dos separaciones
    str_count <- sapply(strsplit(names(datos_HOR), '_'), length)

    if (any(str_count > 2)){
      temp = names(datos_HOR)[which(str_count > 2)]
      target <- gsub('H[0-9]_','',temp)
      target <- gsub('_','-',target)
      horizons <- sapply(strsplit(temp, "_", fixed = TRUE), "[", 1)
      list_final <- list(horizons,rep('_', length(target)),target)
      list_final <- do.call('paste0', list_final)
      names(datos_HOR)[which(str_count > 2)] <- list_final
    }

    # Remover column de color matriz humedo (si existe) que se encuentra en la base de datos de observaciones
    datos_HOR <- datos_HOR %>% select_if(!endsWith(names(.),'_COLOR_MATRIZ_HUMEDO1'))
    datos_HOR <- datos_HOR %>% select_if(!endsWith(names(.),'CO-H1'))

    # Reemplazar patron .*_
    indx <- gsub(".*_", "", names(datos_HOR))

    # Configurar PRU
    PRU <- setNames(
      lapply(split(colnames(datos_HOR), indx), function(x) datos_HOR[x]),
      paste('../data', sort(unique(indx)), sep="_"))

    # Datos PRU
    datos_PRU  <- data.frame(0)
    for(i in seq_along(PRU)){
      temp <- gather(PRU[[i]])
      names(temp) <- c("HORIZONTE",strsplit(names(PRU[[i]])[1],"_")[[1]][2])
      datos_PRU <- data.frame(temp,datos_PRU)
    }

    # Crear ID perfil x horizonte
    ID_PERFIL_HOR <- paste(rep(row.names(datos_HOR),ncol(PRU[[2]])),gl(ncol(PRU[[2]]),nrow(PRU[[1]])),sep="_")

    # Unir perfiles x horizonte + datos
    datos_PERxHOR <- data.frame(ID_PERFIL_HOR=ID_PERFIL_HOR,HORIZONTE=gl(6,dim(datos_HOR)[1]),datos_PRU)

    # Remover columnas sobrantes
    remover <- seq(3,dim(datos_PERxHOR)[2],2)

    # Creat dataset final
    datos_vertical <- datos_PERxHOR[,-c(remover)]
    datos_vertical <- data.frame(ID_PERFIL=row.names(datos_HOR),datos_vertical)
    datos_vertical <- datos_vertical[order(datos_vertical$ID_PERFIL_HOR),]
    datos_vertical <- datos_vertical %>% select_if(!names(.) %in% c('PERFIL'))

    # Renombrar columnas
    PROFUNDIDADINICIAL <- dplyr::select(datos_vertical, matches('INI'))
    PROFUNDIDADFINAL <- dplyr::select(datos_vertical, matches('FIN'))
    ESPESOR <- dplyr::select(datos_vertical, matches('ESPESOR'))

    datos_vertical_cov <- data.frame(PROFUNDIDADINICIAL, PROFUNDIDADFINAL, ESPESOR)
    names(datos_vertical_cov) <- c('PROFUNDIDADINICIAL', 'PROFUNDIDADFINAL', 'ESPESOR')

    # Adicionar variables continuas iterativamente
    for (varcon in project.vars.continuas){
      # Buscar columna relacionada con la variable continua
      datos_vertical_cov[,varcon] <- dplyr::select(datos_vertical, matches(varcon))
      # Corregir valores de decimales
      datos_vertical_cov[,varcon] <- sub(',','.', datos_vertical_cov[,varcon], fixed = TRUE)
      # Reemplazar valores pH de cero a NA
      datos_vertical_cov[which(datos_vertical_cov[,varcon] %in% c(0,'Sin dato')), varcon] <- NA
    }

    # Crear archivo analisis
    datos_vertical <- data.frame(datos_vertical[,1:3],datos_vertical_cov)

    ## Arreglar valores de profundidades
    # Arreglar las profundidad inicial
    datos_vertical[, 'PROFUNDIDADINICIAL'] <- sapply(datos_vertical[, 'PROFUNDIDADINICIAL'], as.numeric)

    # Corregir valores de profundidad final con valores iguales a X, reemplazando por 10 (según FAO) - El valor X es común en las bases de datos
    datos_vertical[which(datos_vertical$PROFUNDIDADFINAL %in% c('X','Sin dato','N/A')),'PROFUNDIDADFINAL'] <- datos_vertical[which(datos_vertical$PROFUNDIDADFINAL %in% c('X','Sin dato','N/A')),'PROFUNDIDADINICIAL'] + 10

    # Arreglar las profundidad inicial
    datos_vertical[, 'PROFUNDIDADFINAL' ] <- sapply(datos_vertical[, 'PROFUNDIDADFINAL'], as.numeric)

    # Corregir valores de profundidad inicial sin datos de profundidad final
    datos_vertical[which(!is.na(datos_vertical$PROFUNDIDADINICIAL) & is.na(datos_vertical$PROFUNDIDADFINAL)), 'PROFUNDIDADINICIAL'] <- NA

    # Recalcular espesor
    datos_vertical[,'ESPESOR'] <- as.numeric(datos_vertical[,'PROFUNDIDADFINAL']) - as.numeric(datos_vertical[,'PROFUNDIDADINICIAL'])

    # Corregir espesores iguales a cero
    perfilid_adicionar = unique(datos_vertical[which(datos_vertical$ESPESOR <= '0'), 'ID_PERFIL'])
    value_adicionar = datos_vertical[which(datos_vertical$ESPESOR <= '0'), 'PROFUNDIDADINICIAL']

    for (i in seq_along(perfilid_adicionar)){
      inicial_temp <- datos_vertical[which(datos_vertical$ID_PERFIL == perfilid_adicionar[i]), 'PROFUNDIDADINICIAL']
      ordenada.list.inicial <- sort(inicial_temp)
      idx.cambiar <- ordenada.list.inicial > value_adicionar
      reemplazar.list.inicial <- c(ordenada.list.inicial[!idx.cambiar],(ordenada.list.inicial[idx.cambiar]+value_adicionar[i]))
      if (any(is.na(inicial_temp))){
        NAs_obs <- length(which(is.na(inicial_temp)))
        reemplazar.list.inicial <- c(reemplazar.list.inicial,rep(NA, NAs_obs))
      }
      datos_vertical[which(datos_vertical$ID_PERFIL == perfilid_adicionar[i]), 'PROFUNDIDADINICIAL'] <- reemplazar.list.inicial

      final_temp <- datos_vertical[which(datos_vertical$ID_PERFIL == perfilid_adicionar[i]), 'PROFUNDIDADFINAL']
      reemplazar.list.final <- as.numeric(final_temp[!is.na(final_temp)]) + value_adicionar[i]
      if (any(is.na(final_temp))){
        NAs_obs <- length(which(is.na(final_temp)))
        reemplazar.list.final <- c(reemplazar.list.final,rep(NA, NAs_obs))
      }
      datos_vertical[which(datos_vertical$ID_PERFIL == perfilid_adicionar[i]), 'PROFUNDIDADFINAL'] <- reemplazar.list.final
    }

    # Recalcular espesor
    datos_vertical[,'ESPESOR'] <- as.numeric(datos_vertical[,'PROFUNDIDADFINAL']) - as.numeric(datos_vertical[,'PROFUNDIDADINICIAL'])

    ### COLOR NO ES NECESARIO PARA CESAR MAGDALENA PERO ES POSIBLE QUE PARA OTRAS ZONAS (SE SUGIERE ACTUALIZAR PARA FUTURAS VERSIONES)
    ## Agregar columnas color #TODO: hay todavia inconsistencias en columna V_H1, automatizar
    #datos_vertical$COLOR <- as.character(datos_vertical$COLOR)
    #datos_vertical$vc_H1 <- ifelse(grepl('2.5/',datos_vertical$COLOR),yes=substr(datos_vertical$COLOR, nchar(datos_vertical$COLOR)-4, nchar(datos_vertical$COLOR)),
    #                         no = substr(datos_vertical$COLOR, nchar(datos_vertical$COLOR)-2, nchar(datos_vertical$COLOR)))
    #datos_vertical$HUE_H1 <- substr(datos_vertical$COLOR, 1,nchar(datos_vertical$COLOR)-nchar(datos_vertical$vc_H1))
    #datos_vertical <- tidyr::separate(datos_vertical,vc_H1,sep="/",into = c("V_H1","C_H1"))
    #datos_vertical$V_H1 <- as.numeric(datos_vertical$V_H1)
    #datos_vertical$C_H1 <- as.numeric(datos_vertical$C_H1)

    # Arreglo de varibles categoricas (opcional, sin embargo la verticalizacion esta enfocada para variables continuas)
    ## Cambiar valores NAs en columna Textura
    #datos_vertical[which(datos_vertical$TEXTURA %in% c('Sin dato','N/AN')),'TEXTURA'] <- NA
    ## Cambiar valores X en columna Nomenclatura
    #datos_vertical[which(datos_vertical$NOMENCLATURA == 'X'),'NOMENCLATURA'] <- NA

    # Export datos verticalizados
    write.table(datos_vertical, verticalizada.archivo, row.names=F, sep=',')

    # ------------------------------------------------------- #
    # Mensajes de salida
    # ------------------------------------------------------- #
    cat(paste0('### RESULTADO: La base de datos de ',tipo,' del proyecto ',basename(project.name),' fue procesada con exito y contiene ',dim(datos_HOR)[1], ' perfiles','\n','\n'))
  } else {
    verticalizada <- read.csv(verticalizada.archivo, sep=',')
    perfiles <- length(unique(verticalizada$ID_PERFIL))
    cat(paste0('### RESULTADO: La base de datos de ',tipo,' del proyecto ',basename(project.name),' ya existe (se recomienda inspeccionarla) y contiene ',perfiles, ' perfiles','\n','\n'))
  }

  #estimar tiempo de procesamiento total
  timeEnd = Sys.time()
  print(round(difftime(timeEnd, timeStart, units='mins'),2))
}