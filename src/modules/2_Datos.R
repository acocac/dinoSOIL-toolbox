#############################################################################
# titulo        : Datos de entrada (matriz) y covariables ambientales (GeoTIFF);
# proposito     : Generar datos de entrada (matriz) y raster multibanda con las covariables ambientales (GeoTIFF);
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogotá, Colombia / Actualizado por ACC en Octubre 2020;;
# entrada       : Base de datos verticalizada;
# salida        : Datos de entrada y GeoTIFF con las covariables ambientales para su uso en la predicción;
# observaciones : ninguna;
##############################################################################

prompt.user.part2 <- function()#get arguments from user
{
  message(prompt="Indique el nombre del archivo EXCEL con las variables categoricas y coordenadas:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el nombre de la pestana del archivo EXCEL con las variables categoricas y coordenadas:>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  message(prompt="Indique el nombre de la columna que tiene el ID de los perfiles del archivo EXCEL las variables categoricas y coordenadas:>>> ")
  c <- readLines(n = 1)
  c <- gsub("\\\\", "/", c)

  message(prompt="Indique el nombre de la carpeta con el limite a usar dentro del directorio de entrada/2_limite:>>> ")
  d <- readLines(n = 1)
  d <- gsub("\\\\", "/", d)

  newlist = list(a, b, c, d)
  return(newlist)
}

Datos <- function(filename, hoja, columna, limite.carpeta){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  suppressMessages(library(pacman))
  suppressMessages(pacman::p_load(readxl, tidyr, plyr, dplyr, raster, GSIF, aqp, sf, rgdal,
                                  smoothr, gdalUtilities, magrittr, stringr, caret))

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0_CargarConfig.R'))
  source(paste0(r.dir,'/functions/2_Ponderado.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  project.folder <- conf.args[['proyecto.carpeta']]
  project.name <- sapply(strsplit(project.folder, '_'), tail, 1)
  project.vars.categoricas <- conf.args[['vars.categoricas']]
  project.vars.categoricas = unlist(strsplit(project.vars.categoricas,';'))
  project.vars.continuas <- conf.args[['vars.continuas']]
  project.vars.continuas <- unlist(strsplit(project.vars.continuas,';'))
  project.vars.all <- c(project.vars.categoricas, project.vars.continuas)

  project.covars.vector <- conf.args[['covariables.vector']]
  project.covars.vector <- unlist(strsplit(project.covars.vector,';'))
  project.covars.raster <- conf.args[['covariables.raster']]
  project.covars.raster <- unlist(strsplit(project.covars.raster,';'))
  project.covars.list <- c(project.covars.vector, project.covars.raster)

  #configurar los atributos de los archivos vectoriales
  project.vector.atributos <- conf.args[['vector.atributos']]
  project.vector.atributos <- unlist(strsplit(project.vector.atributos,';'))
  names(project.vector.atributos) <- project.covars.vector

  fechas.ndvi.list <- conf.args[['fechas.ndvi']]
  fechas.ndvi.list <- unlist(strsplit(fechas.ndvi.list,';'))
  
  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  in.tb.data <- paste0(project.folder,'/datos/entrada/0_basededatos')
  in.geo.data <- paste0(project.folder,'/datos/entrada/1_covariables')
  in.limite.data <- paste0(project.folder,'/datos/entrada/2_limite')
  out.tb.data <- paste0(project.folder,'/datos/salida/0_matriz')
  out.geo.data <- paste0(project.folder,'/datos/salida/1_covariables')
  dir.create(out.tb.data, recursive = T, mode = "0777", showWarnings = F)
  dir.create(out.geo.data, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(project.folder))

  # Matriz Datos
  matriz.datos.archivo <- paste0( out.tb.data,'/','MatrixDatos.csv')
  if (!file.exists(matriz.datos.archivo)){
    # iniciar el monitoreo tiempo de procesamiento total
    start_time <- Sys.time()

    # Mensaje de estado
    cat(paste0('La matriz de datos no existe, se procede a generarla','\n','\n'))

    # Covariables
    covariables.archivo.stack <- paste0(out.geo.data,'/covariables.tif')
    if (!file.exists(covariables.archivo.stack)){
      cat(paste0('El archivo stack geoTIFF de las covariables no existe, se requiere generarlo para extraer la matriz de datos','\n','\n'))
      # Cargar area limite
      limite_shp <- st_read(paste0(in.limite.data,'/', limite.carpeta))
      if (dim(limite_shp)[1] > 1){
        limite_shp$id <- 0
        limite_shp <- limite_shp %>% dplyr::group_by(id) %>% dplyr::summarize()
      }

      # Cargar DEM (referencia de la resolución espacial)
      DEM_rast_res <- raster(paste0(in.geo.data,'/raster/dem/original/dem.tif'))
      names(DEM_rast_res) <- 'dem'
      resolucion_dem = res(DEM_rast_res)[1]

      # Crear stack de derivados del DEM
      DEMderivados_lista <- list.files(path=paste0(in.geo.data,'/raster/dem/derivados'), pattern='tif', all.files=FALSE, full.names=TRUE,recursive=TRUE)
      if (length(DEMderivados_lista) > 0){
        DEMderivados_rast_res <- stack(DEMderivados_lista)
      }

      ## Procesar datos espaciales #
      ## Derivados DEM
      #if ('DEM' %in% project.covars.list){
      #  out.dir <- paste0(in.geo.data,'/raster/dem/stack')
      #  dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
      #  covariable.archivo <- paste0(out.dir,'/dem_cov.tif')
      #  if (!file.exists(covariable.archivo)){
      #    cat(paste0('El archivo stack geoTIFF de la variable DEM y derivados no existe, se requiere generarlo','\n','\n'))
      #    DEM_derivado <- stack(list.files(path=paste0(in.geo.data,'/raster/dem/derivados'), pattern='tif', all.files=FALSE, full.names=TRUE,recursive=TRUE))
      #    DEM_rast_res <- stack(dem, DEM_derivado)
      #    names(DEM_rast_res)[1] <- 'dem'
      #    DEM_rast_res <- crop(DEM_rast_res,limite_shp)
      #    writeRaster(DEM_rast_res, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
      #    saveRDS(names(DEM_rast_res),file=gsub('.tif','.rds',covariable.archivo))
      #  } else{
      #    cat(paste0('El archivo stack geoTIFF de la variable DEM y derivados existe, se va agregar al covariable','\n','\n'))
      #    DEM_rast_res <- stack(covariable.archivo)
      #    nombres_DEM <- readRDS(gsub('.tif','.rds',covariable.archivo))
      #    names(DEM_rast_res) <- nombres_DEM
      #  }
      #}

      # Vegetacion 
      if ('NDVI' %in% project.covars.list){
        out.dir <- paste0(in.geo.data,'/raster/ndvi')
        dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
        covariable.archivo <- paste0(out.dir,'/ndvi',fechas.ndvi.list[1],'_',fechas.ndvi.list[2],'.tif')
        if (!file.exists(covariable.archivo)){
          cat(paste0('El archivo geoTIFF de la variable NDVI no existe, se requiere generarlo','\n','\n'))

          # # Load libraries
          pckg <- c('reticulate', 'remotes', 'mapview',
                    'sf', 'geojsonio', 'googledrive')

          usePackage <- function(p) {
            if (!is.element(p, installed.packages()[,1]))
              install.packages(p, dep = TRUE)
            require(p, character.only = TRUE)
          }

          lapply(pckg,usePackage)

          #remotes::install_github("r-spatial/rgee")
          require(rgee)
          
          ## It is necessary just once
          #ee_install()
          ##if Error in reticulate::py_discover_config()
          #ee_install_upgrade(version = "0.1.224")
          ##if Error in path.expand(path) : invalid 'path' argument (windows OS)
          # Sys.setenv(RETICULATE_PYTHON = "x") donde x es la ruta donde esta miniconda ambiente r-reticulate por ejemplo "C:/Users/alejandro.coca/AppData/Local/r-miniconda/envs/r-reticulate"
          # despues de instalar reiniciar R, para qeu se configura el nuevo ambiente si no cambiar como
          # Sys.setenv(RETICULATE_PYTHON = "x") donde x es la ruta donde esta miniconda ambiente rgee por ejemplo "C:/Users/alejandro.coca/AppData/Local/r-miniconda/envs/rgee"

          # Initialize Earth Engine!
          ee_Initialize()

          #### Funciones ####
          maskL8sr <- function(image){
            cloudShadowBitMask = bitwShiftL(1, 3);
            cloudsBitMask = bitwShiftL(1, 5);
            qa = image$select('pixel_qa');
            mask = qa$bitwiseAnd(cloudShadowBitMask)$eq(0)$And(qa$bitwiseAnd(cloudsBitMask)$eq(0))
            return(image$updateMask(mask))
          }

          #### Criterios de busqueda ####
          shp <- limite_shp %>% sf_as_ee()
          
          start <- ee$Date(fechas.ndvi.list[1])
          finish <- ee$Date(fechas.ndvi.list[2])

          #### Busqueda en la coleccion L8 ####
          filteredCollection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$
            filterBounds(shp)$
            filterDate(start, finish)$
            filterMetadata('CLOUD_COVER', 'less_than',30)$
            map(maskL8sr)

          #### Mediana de la coleccion ####
          median <- filteredCollection$reduce(ee$Reducer$median())

          #### NDVI ####
          getNDVI <- function(image) {
            return(image$normalizedDifference(c("B5_median", "B4_median")))
          }
          ndvi <- getNDVI(median)

          resolucion_ndvi = filteredCollection$first()$select('B5')$projection()$nominalScale()$getInfo()

          if (resolucion_dem > resolucion_ndvi){
            resolucion <- resolucion_dem
          } else{
            resolucion <- resolucion_ndvi
          }

          #### Exportar drive ####
          task_img <- ee_image_to_drive(
            image = ndvi,
            fileFormat = "GEO_TIFF",
            fileNamePrefix = project.name,
            folder = project.name,
            region = shp$geometry(),
            scale = resolucion
          )

          task_img$start()
          ee_monitoring(task_img)

          imgloc <- ee_drive_to_local(task = task_img)

          NDVI_rast <- raster(imgloc)
          NDVI_rast_res <- projectRaster(NDVI_rast,DEM_rast_res,method="bilinear")

          # Guardar archivo
          writeRaster(NDVI_rast_res, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
        } else{
          cat(paste0('El archivo geoTIFF de la variable NDVI existe, se va agregar al covariable','\n','\n'))
          NDVI_rast_res <- raster(covariable.archivo)
        }
        names(NDVI_rast_res) <- 'ndvi'
      }

      if (length(project.covars.vector) > 0){
        for (covar in project.covars.vector){
          out.dir <- paste0(in.geo.data,'/raster/',covar)
          dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
          covariable.archivo <- paste0(out.dir,'/',covar,'.tif')
          if (!file.exists(covariable.archivo)){
            cat(paste0('El archivo geoTIFF de la variable ', covar, ' no existe, se requiere generarlo','\n','\n'))
            # Lista de shapefiles
            covar_files <- list.files(path = paste0(in.geo.data,'/vector/',covar), pattern = "\\.shp$", full.names=TRUE)

            # Atributo
            covar_atributo <- project.vector.atributos[[covar]]

            if (length(covar_files) > 1){
              covar_list <- lapply(covar_files, readOGR)

              # Seleccionar y procesar variable objetivo
              covar_list_target <- lapply(covar_list, "[", c(covar_atributo))
              covar_combined <- do.call(what = rbind, args=covar_list_target)
              covar_intersect <- raster::intersect(covar_combined, limite_shp)
              covar_intersect@data[[covar_atributo]] = gsub(",", "", covar_intersect@data[[covar_atributo]])
              covar_intersect <- aggregate(covar_intersect, covar_atributo)
              covar_vector <- fill_holes(covar_intersect, units::set_units(1, km^2))
            } else {
              covar_vector <- readOGR(paste0(in.geo.data,'/vector/',covar))
              covar_vector <- raster::intersect(covar_vector, limite_shp)
            }

            ## Rasterizar capa
            covar_vector <- spTransform(covar_vector, CRS=projection(DEM_rast_res))
            covar_vector$grupos <- as.factor(covar_vector@data[[covar_atributo]])
            covar_raster <- gRasterize(covar_vector, DEM_rast_res, 'grupos')
            covar_raster_ngb <- resample(covar_raster, DEM_rast_res, method='ngb')
            values(covar_raster_ngb) <- round(values(covar_raster_ngb),0)

            names(covar_raster_ngb) <- covar
            assign(paste0(covar, '_rast_res'), covar_raster_ngb)

            # Guardar archivo
            writeRaster(covar_raster_ngb, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
          } else{
            cat(paste0('El archivo geoTIFF de la variable ',  covar, ' existe, se va agregar al covariable','\n','\n'))
            covar_raster_ngb <- raster(covariable.archivo)
            names(covar_raster_ngb) <- covar
            assign(paste0(covar, '_rast_res'), covar_raster_ngb)
          }
          #names(eval(paste0(covar, '_raster_res'))) <- covar
        }
      }

      # Generar COV tif
      list_obj = objects(pattern="*_rast_res")
      list_covs <- lapply(list_obj, function(x) get(x))
      covariables.nombres <- unlist(lapply(list_obj, function(x) names(get(x))))

      # Crear stack
      covariables <- stack(list_covs)
      covariables <- crop(covariables,limite_shp)
      covariables <- mask(covariables,limite_shp)
      names(covariables) <- covariables.nombres

      # Exportar GeoTIFF covariables
      writeRaster(covariables,covariables.archivo.stack, drivers = 'GeoTIFF', overwrite=TRUE)
      saveRDS(names(covariables),str_replace(covariables.archivo.stack,'.tif','.rds'))
    } else{
      cat(paste0('El archivo stack geoTIFF de las covariables existe, se va usar para extraer la matriz de datos','\n','\n'))
      covariables <- stack(covariables.archivo.stack)
      covariables.nombres <-readRDS(str_replace(covariables.archivo.stack,'.tif','.rds'))
      names(covariables) <- covariables.nombres
    }

    # Procesar por variable #
    if (length(project.vars.continuas) > 0){
      project.ajuste.continuas <- conf.args[['ajuste.continuas']]
      project.ajuste.continuas <- unlist(strsplit(project.ajuste.continuas,';'))
      prof1 <- strsplit(project.ajuste.continuas[1],'-')
      prof2 <- strsplit(project.ajuste.continuas[2],'-')

      varcon.todas <- list()
      for (varcon in project.vars.continuas){
        out.dir = paste0(in.tb.data,'/derivados/interpolados')
        dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
        interpolado.archivo <- paste0(out.dir,'/BD_',project.name,'_',varcon,'.csv')
        if (!file.exists(interpolado.archivo)){
          cat(paste0('El archivo tabular de la interpolación de la variable ', varcon, ' no existe, se requiere generarlo','\n','\n'))
          # ------------------------------------------------------- #
          # Carga y preparacion de los datos
          # ------------------------------------------------------- #
          data_obs <- read.csv(paste0(in.tb.data,'/derivados/verticalizado/BDO_',project.name,'_Vert.csv'), sep=',',, na='NA')
          data_per <- read.csv(paste0(in.tb.data,'/derivados/verticalizado/BDP_',project.name,'_Vert.csv'), sep=',',, na='NA')
          data <- rbind.fill(data_obs,data_per)

          names(data)[1:5] <- c("profileId","HorID","HorNO","top","bottom","depth")

          #preprocesamiento
          data[,varcon] = as.numeric(as.character(data[,varcon]))
          data <- data %>% filter_at(vars(top,bottom),all_vars(!is.na(.)))

          #agregar aqui posibles valores ilogicos que puedan afectar los analisis
          if (varcon == 'pH'){
            data[which(data[,varcon] > 11),varcon] = data[which(data[,varcon] > 11),varcon] / 10
          }

          #crear dataset sitios
          sites <- data.frame(profileId=unique(data$profileId))

          #--------------------------------------------------------
          #OPCION 1: FUNCIONES DE SUAVIZADO DE AREA EQUIVALENTE
          #--------------------------------------------------------
          ##Crear objeto SoilProfileCollection
          dat_aqp <- data
          names(dat_aqp)
          depths(dat_aqp) <- profileId ~ top + bottom
          site(dat_aqp) <- sites

          p1_ini = as.numeric(prof1[[1]][1])
          p1_fin = as.numeric(prof1[[1]][2])
          p2_ini = as.numeric(prof1[[1]][1])
          p2_fin = as.numeric(prof2[[1]][2])

          # Hallar el valor deseado con splines
          {
          try(varcon.p1 <- mpspline(dat_aqp, varcon, d = t(c(p1_ini, p1_fin))))
          try(varcon.p2 <- mpspline(dat_aqp, varcon, d = t(c(p2_ini, p2_fin))))
          dat_spline <- data.frame(dat_aqp@site$profileId,
                                   varcon.p1$var.std[,1],
                                   varcon.p2$var.std[,1])
          names(dat_spline) <- c('profileId',paste0(varcon, '.', project.ajuste.continuas[1],'_spline'),paste0(varcon, '.', project.ajuste.continuas[2],'_spline'))
          int_m1 <- plyr::join(sites,dat_spline,by="profileId")
          }

          #-------------------------------------------------------------------------------
          #OPCION 2: SUMA PONDERADA (ASUMIENDO EL ESPESOR DEL HORIZONTE COMO PONDERADOR)
          #-------------------------------------------------------------------------------
          # Intervalos
          assign(paste0('int_m2_', as.character(p1_ini),'_', as.character(p1_fin)), Ponderado(data, varcon, p1_ini, p1_fin))
          assign(paste0('int_m2_', as.character(p2_ini),'_', as.character(p2_fin)), Ponderado(data, varcon, p2_ini, p2_fin))

          #-------------------------------
          #RESULTADO FINAL
          #-------------------------------
          list_obj = objects(pattern="*int_m")
          dfs <- lapply(list_obj, function(x) get(x))

          datos_varcon <- join_all(dfs, "profileId")
          varcon.todas[[varcon]] <- datos_varcon
          write.table(datos_varcon, interpolado.archivo, row.names = F, sep=',')
        } else {
          cat(paste0('El archivo tabular de la interpolación de la variable ', varcon,' existe, se va adicionar a la matriz','\n','\n'))
          datos_varcon <- read.csv(interpolado.archivo)
          varcon.todas[[varcon]] <- datos_varcon
        }
      }
    }

    var_con_df <- join_all(varcon.todas, by = 'profileId')

    # Unir base de datos
    ##Carga de base con variables a modelar
    datos_modelos <- data.frame(read_excel(paste0(in.tb.data,'/originales/',filename), sheet = hoja, na = "N/A"))

    if (length(project.vars.continuas) > 0){
      ##renombrar columna
      names(var_con_df)[which(names(var_con_df) == 'profileId')] = columna
      ##Union de valores de pH a base de variables taxonomicas y coordenadas
      datos_final <- plyr::join(datos_modelos, var_con_df, by=columna, type = "inner")
    } else{
      datos_final <- datos_modelos
    }

    # Factorizar columnas
    columnas_factores <- c('EPIPEDON', 'ENDOPEDON', 'ORDEN', 'SUBORDEN', 'SUBGRUPO', 'GRANGRUPO', 'FAMILIA_TE')
    datos_final <- datos_final %<>% mutate_at(columnas_factores, funs(factor(.)))

    dem <- raster(paste0(in.geo.data,'/raster/dem/original/dem.tif'))

    datos_sp <-st_as_sf(datos_final, coords=c("LONGITUD","LATITUD"))
    proyeccion <- CRS("+proj=longlat +datum=WGS84")
    st_crs(datos_sp) <- proyeccion
    datos_sp_cov <- st_transform(datos_sp, projection(dem))

    # Extraer valores metodo normal raster
    matriz_datos <- cbind(datos_final, raster::extract(covariables, datos_sp_cov))

    # Eliminar observaciones cero varianza
    index_cov = dim(datos_final)[2]+1
    matriz_datos <- matriz_datos %>% filter_at(vars(index_cov), all_vars(!is.na(.)))

    # Exportar Matriz de Datos
    write.table(matriz_datos, paste0(out.tb.data,'/','MatrixDatos.csv'), row.names = F, sep=',')

    #estimar tiempo de procesamiento total
    print(Sys.time() - start_time)
  } else{
    # Mensaje de estado
    cat(paste0('La matriz de datos existe, se puede usar para los otros componentes','\n','\n'))
  }
}