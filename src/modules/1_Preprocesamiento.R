#############################################################################
# titulo        : Verticalizacion bases de datos de suelo;
# proposito     : Verticalizar bases de datos de suelo proveidas en su formato horizontal;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

Preprocesamiento <- function(tipo, filename, hoja, columna){
  if (missing(filename))
    stop("Necesita especificar la base de datos para verticalizar.")

  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg <- c('readxl', 'tidyr')

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
  in.data <- paste0(project.folder,'/datos/entrada/0_basededatos')
  out.data <- paste0(project.folder,'/datos/salida/0_basededatos')
  dir.create(out.data, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(project.folder))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  # Base de datos (horizontal)
  datos_HOR <- data.frame(read_excel(paste0(in.data,'/',filename), sheet = hoja, na = "N/A"))
  datos_HOR <- datos_HOR[!duplicated(datos_HOR[,columna]),]
  row.names(datos_HOR) <- datos_HOR[,columna]

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

  # Corregir valores de profundidad inicial sin datos de profundidad final
  datos_vertical[which(!is.na(datos_vertical$PROFUNDIDADINICIAL) & is.na(datos_vertical$PROFUNDIDADFINAL)), 'PROFUNDIDADINICIAL'] <- NA

  # Corregir valores de profundidad final con valores iguales a X, reemplazando por 10 (según FAO) - El valor X es común en las bases de datos
  datos_vertical[which(datos_vertical$PROFUNDIDADFINAL == 'X'),'PROFUNDIDADFINAL'] <- datos_vertical[which(datos_vertical$PROFUNDIDADFINAL == 'X'),'PROFUNDIDADINICIAL'] + 10

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

  # Remover columna RANGOPROFUNDIDAD
  datos_vertical[,'RANGOPROFUNDIDAD'] <- NULL

  # Cambiar valores X en columna Nomenclatura
  datos_vertical[which(datos_vertical$NOMENCLATURA == 'X'),'NOMENCLATURA'] <- NA

  # Reemplazar valores pH de cero a NA
  datos_vertical[which(datos_vertical$ph %in% c(0,'Sin dato')),'ph'] <- NA

  # Agregar columnas color #TODO: hay todavia inconsistencias en columna V_H1, automatizar
  datos_vertical$COLOR <- as.character(datos_vertical$COLOR)
  datos_vertical$vc_H1 <- ifelse(grepl('2.5/',datos_vertical$COLOR),yes=substr(datos_vertical$COLOR, nchar(datos_vertical$COLOR)-4, nchar(datos_vertical$COLOR)),
                           no = substr(datos_vertical$COLOR, nchar(datos_vertical$COLOR)-2, nchar(datos_vertical$COLOR)))
  datos_vertical$HUE_H1 <- substr(datos_vertical$COLOR, 1,nchar(datos_vertical$COLOR)-nchar(datos_vertical$vc_H1))
  datos_vertical <- tidyr::separate(datos_vertical,vc_H1,sep="/",into = c("V_H1","C_H1"))
  datos_vertical$V_H1 <- as.numeric(datos_vertical$V_H1)
  datos_vertical$C_H1 <- as.numeric(datos_vertical$C_H1)

  if (tipo == 'PERFILES'){
    file_prefix <- 'BDP'
  } else if (tipo == 'OBSERVACIONES'){
    file_prefix <- 'BDO'
  }

  # Export datos verticalizados
  write.csv2(datos_vertical, paste0(out.data,'/',file_prefix,'_',project.name,'_Vert.csv'), row.names=F)

  # ------------------------------------------------------- #
  # Mensajes de salida
  # ------------------------------------------------------- #
  cat(paste0("### RESULTADO: La base de datos del proyecto ",basename(project.name)," fue procesada con exito y contiene ",dim(datos_HOR)[1], " perfiles"))

}