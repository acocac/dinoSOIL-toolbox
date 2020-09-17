#############################################################################
# titulo        : Verticalizacion bases de datos de suelo;
# proposito     : Verticalizar bases de datos de suelo proveidas en su formato horizontal;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

Preprocesamiento <- function(filename, hoja, columna){
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
  datos_HOR <- data.frame(read_excel(paste0(in.data,'/',filename), sheet = hoja, na = "N/A"), row.names=columna)

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
  datos_vertical$ID_PERFIL <- row.names(datos_HOR)

  # Export datos verticalizados
  write.csv2(datos_vertical, paste0(out.data,'/BDP_',project.name,'_Vert.csv'), row.names=F)

  # ------------------------------------------------------- #
  # Mensajes de salida
  # ------------------------------------------------------- #
  cat(paste0("### RESULTADO: La base de datos del proyecto ",basename(project.name)," fue procesada con exito y contiene ",dim(datos_HOR)[1], " perfiles"))

}