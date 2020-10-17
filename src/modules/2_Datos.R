#############################################################################
# titulo        : Datos de entrada (matriz) y predicción (GeoTIFF;
# proposito     : Generar datos de (matriz) y predicción (GeoTIFF);
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogotá, Colombia / Actualizado por ACC en Octubre 2020;;
# entrada       : Base de datos verticalizada;
# salida        : Datos de entrada y GeoTIFF para predicción;
# observaciones : ninguna;
##############################################################################

Datos <- function(){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg <- c('readxl', 'tidyr', 'raster', 'GSIF',
            'aqp', 'plyr')

  usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }
  
  lapply(pckg,usePackage)

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/2a_phPonderado.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  project.folder <- conf.args[[1]]
  project.name <- sapply(strsplit(project.folder, '_'), tail, 1)
  project.vars.categoricas <- conf.args[[2]]
  project.vars.categoricas = unlist(strsplit(project.vars.categoricas,';'))
  project.vars.continuas <- conf.args[[3]]
  project.vars.continuas <- unlist(strsplit(project.vars.continuas,';'))
  project.vars.all <- c(project.vars.categoricas, project.vars.continuas)
  project.covars.list <- conf.args[[4]]
  project.covars.list <- unlist(strsplit(project.covars.list,';'))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  in.tb.data <- paste0(project.folder,'/datos/entrada/0_basededatos')
  in.geo.data <- paste0(project.folder,'/datos/entrada/1_covariables')
  out.tb.data <- paste0(project.folder,'/datos/salida/0_matriz')
  out.geo.data <- paste0(project.folder,'/datos/salida/1_covariables')
  dir.create(out.tb.data, recursive = T, mode = "0777", showWarnings = F)
  dir.create(out.geo.data, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(project.folder))

  # Covariables
  stack.dir = paste0(in.geo.data,'/raster/stack')
  dir.create(stack.dir, recursive = T, mode = "0777", showWarnings = F)
  covariables.archivo.stack <- paste0(stack.dir,'/covariables.tif')
  if (!file.exists(covariables.archivo.stack)){
    # Cargar area limite
    limite_shp <- st_read(paste0(in.geo.data,'/vector/limite'))
    if (dim(limite_shp)[1] > 1){
      limite_shp$id <- 0
      limite_shp <- limite_shp %>% group_by(id) %>% summarize()
    }

    # Cargar DEM (referencia de la resolución espacial)
    dem <- raster(paste0(in.geo.data,'/raster/separados/dem/original/DEM_PT_2020.tif'))

    # Procesar datos espaciales #
    # Derivados DEM
    if ('DEM' %in% project.covars.list){
      out.dir <- paste0(in.geo.data,'/raster/separados/dem/stack')
      dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
      covariable.archivo <- paste0(out.dir,'/dem_cov.tif')
      if (!file.exists(covariable.archivo)){
        cat(paste0('El archivo stack geoTIFF de la variable DEM y derivados no existe, se requiere generarlo','\n','\n'))
        DEM_derivado <- stack(list.files(path=paste0(in.geo.data,'/raster/separados/dem/derivados'), pattern='tif', all.files=FALSE, full.names=TRUE,recursive=TRUE))
        DEM_rast_res <- stack(dem, DEM_derivado)
        names(DEM_rast_res)[1] <- 'dem'
        DEM_rast_res <- crop(DEM_rast_res,limite_shp)
        writeRaster(DEM_rast_res, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
        saveRDS(names(DEM_rast_res),file=gsub('.tif','.rds',covariable.archivo))
      } else{
        cat(paste0('El archivo stack geoTIFF de la variable DEM y derivados existe, se va agregar al covariable','\n','\n'))
        DEM_rast_res <- stack(covariable.archivo)
        nombres_DEM <- readRDS(gsub('.tif','.rds',covariable.archivo))
        names(DEM_rast_res) <- nombres_DEM
        print(names(DEM_rast_res))
      }
    }

    # Vegetación
    if ('NDVI' %in% project.covars.list){
      out.dir <- paste0(in.geo.data,'/raster/separados/ndvi')
      dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
      covariable.archivo <- paste0(out.dir,'/ndvi.tif')
      if (!file.exists(covariable.archivo)){
        cat(paste0('El archivo geoTIFF de la variable NDVI no existe, se requiere generarlo','\n','\n'))
        remotes::install_github("r-spatial/rgee")

        # # Load libraries
        pckg <- c('reticulate', 'remotes', 'rgee', 'mapview',
                  'sf', 'geojsonio', 'googledrive')

        usePackage <- function(p) {
          if (!is.element(p, installed.packages()[,1]))
            install.packages(p, dep = TRUE)
          require(p, character.only = TRUE)
        }

        lapply(pckg,usePackage)

        ## It is necessary just once
        #ee_install()
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

        collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1")
        start <- ee$Date("2019-01-01")
        finish <- ee$Date("2020-08-30")

        #### Busqueda en la coleccion L8 ####
        filteredCollection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_SR")$
          filterBounds(shp)$
          filterDate(start, finish)$
          filterMetadata('CLOUD_COVER', 'less_than',30)$
          map(maskL8sr)

        #### Mediana de la coleccion ####
        median <- filteredCollection$reduce(ee$Reducer$median())
        vizParams <- list(
          bands = c("B5_median", "B4_median", "B3_median"),
          min = 5000,
          max = 15000,
          gamma = 1.3
        )

        #### NDVI ####
        getNDVI <- function(image) {
          return(image$normalizedDifference(c("B5_median", "B4_median")))
        }
        ndvi <- getNDVI(median)

        resolution = filteredCollection$first()$select('B5')$projection()$nominalScale()$getInfo()

        #### Exportar drive ####
        task_img <- ee_image_to_drive(
          image = ndvi,
          fileFormat = "GEO_TIFF",
          fileNamePrefix = project.name,
          folder = project.name,
          region = shp$geometry(),
          scale = resolution
        )

        task_img$start()
        ee_monitoring(task_img)

        imgloc <- ee_drive_to_local(task = task_img)

        NDVI_rast <- raster(imgloc)
        NDVI_rast_res <- projectRaster(NDVI_rast,dem,method="bilinear")
        NDVI_rast_res <- crop(NDVI_rast_res,limite_shp)
        # Guardar archivo
        writeRaster(NDVI_rast_res, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
      } else{
        cat(paste0('El archivo geoTIFF de la variable NDVI existe, se va agregar al covariable','\n','\n'))
        NDVI_rast_res <- raster(covariable.archivo)
      }
      names(NDVI_rast_res) <- 'ndvi'
    }

    if ('clima' %in% project.covars.list){
      out.dir <- paste0(in.geo.data,'/raster/separados/clima')
      dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
      covariable.archivo <- paste0(out.dir,'/clima.tif')
      if (!file.exists(covariable.archivo)){
        cat(paste0('El archivo geoTIFF de la variable clima no existe, se requiere generarlo','\n','\n'))
        # Lista de shapefiles
        clima_files <- list.files(path = paste0(in.geo.data,'/vector/clima'), pattern = "\\.shp$", full.names=TRUE)
        clima_list <- lapply(clima_files, readOGR)

        # Seleccionar y procesar variable objetivo
        clima_list_target <- lapply(clima_list, "[", c('Denominaci'))
        clima_combined <- do.call(what = rbind, args=clima_list_target)
        clima <- raster::intersect(clima_combined, limite_shp)
        clima@data[['Denominaci']] = gsub(",", "", clima@data[['Denominaci']])
        clima <- aggregate(clima, 'Denominaci')
        clima <- fill_holes(clima, units::set_units(1, km^2))

        ## Rasterizar capa
        clima <- spTransform(clima, CRS=projection(dem))
        clima$denom_char <- as.factor(clima$Denominaci)
        clima_rast <- gRasterize(clima, dem, 'denom_char')
        clima_rast_res <- resample(clima_rast, dem, method="ngb")
        values(clima_rast_res) <- round(values(clima_rast_res),0)

        clima_rast_res <- crop(clima_rast_res,limite_shp)

        # Guardar archivo
        writeRaster(clima_rast_res, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
      } else{
        cat(paste0('El archivo geoTIFF de la variable clima existe, se va agregar al covariable','\n','\n'))
        clima_rast_res <- raster(covariable.archivo)
      }
      names(clima_rast_res) <- 'clima'
    }

    if ('relieve' %in% project.covars.list){
      out.dir <- paste0(in.geo.data,'/raster/separados/relieve')
      dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
      covariable.archivo <- paste0(out.dir,'/relieve.tif')
      if (!file.exists(covariable.archivo)){
        cat(paste0('El archivo geoTIFF de la variable relieve no existe, se requiere generarlo','\n','\n'))
        relieve <- readOGR(paste0(in.geo.data,'/vector/geomorfologia'))
        relieve <- spTransform (relieve, CRS=projection(dem))
        relieve$TIPO_RELIE <- as.character(relieve$TIPO_RELIE)
        relieve_rast <- gRasterize(terrerno, dem, 'TIPO_RELIE')
        relieve_rast_res <- resample(relieve_rast, dem,method='ngb')
        values(relieve_rast_res) <- round(values(relieve_rast_res),0)
        # Guardar archivo
        relieve_rast_res <- crop(relieve_rast_res,limite_shp)

        writeRaster(relieve_rast_res, filename = covariable.archivo, drivers = 'GeoTIFF', overwrite=TRUE)
      } else {
        cat(paste0('El archivo geoTIFF de la variable relieve existe, se va agregar al covariable','\n','\n'))
        relieve_rast_res <- raster(covariable.archivo)
      }
        names(relieve_rast_res) <- 'tipo_relieve'
    }

    # Generar COV tif
    list_obj = objects(pattern="*_rast_res")
    list_covs <- lapply(list_obj, function(x) get(x))
    covariables <- stack(list_covs)
    writeRaster(covariables,covariables.archivo.stack, drivers = 'GeoTIFF', overwrite=TRUE)
  } else{
    covariables <- stack(covariables.archivo.stack)
  }

  # Procesar por variable #
  if ('pH' %in% project.vars.all){
    out.dir = paste0(in.tb.data,'/derivados/interpolados')
    dir.create(out.dir, recursive = T, mode = "0777", showWarnings = F)
    interpolado.archivo <- paste0(out.dir,'/BD_',project.name,'_pH.csv')
    if (!file.exists(interpolado.archivo)){
      cat(paste0('El archivo tabular de la interpolación de la variable pH no existe, se requiere generarlo','\n','\n'))
      # ------------------------------------------------------- #
      # Carga y preparacion de los datos
      # ------------------------------------------------------- #
      data_obs <- read.csv(paste0(in.tb.data,'/derivados/verticalizado/BDO_',project.name,'_Vert.csv'), sep=',',, na='NA')
      data_per <- read.csv(paste0(in.tb.data,'/derivados/verticalizado/BDP_',project.name,'_Vert.csv'), sep=',',, na='NA')
      data <- rbind.fill(data_obs,data_per)

      names(data) <- c("profileId","HorID","HorNO","top","bottom","pH","depth","NOM","HCL","CLTEX")

      #preprocesamiento
      data$pH = as.numeric(as.character(data$pH))
      data <- data %>% filter_at(vars(top,bottom),all_vars(!is.na(.)))
      data[which(data$pH > 11),'pH'] = data[which(data$pH > 11),'pH'] / 10

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

      # Hallar el valor deseado con splines
      {
      try(pH.0_30 <- mpspline(dat_aqp, 'pH', d = t(c(0,30))))
      try(pH.30_100 <- mpspline(dat_aqp, 'pH', d = t(c(30,100))))
      dat_spline <- data.frame(profileId = dat_aqp@site$profileId,
                               pH.0_30_spline = pH.0_30$var.std[,1],
                               pH.30_100_spline = pH.30_100$var.std[,1])
      sites <- plyr::join(sites,dat_spline,by="profileId")
      }

      #-------------------------------------------------------------------------------
      #OPCION 2: SUMA PONDERADA (ASUMIENDO EL ESPESOR DEL HORIZONTE COMO PONDERADOR)
      #-------------------------------------------------------------------------------

      # Intervalos
      wSum_0_30 <- PonderadopH(data,0,30)
      wSum_30_100 <- PonderadopH(data,30,100)

      #-------------------------------
      #RESULTADO FINAL
      #-------------------------------
      dfs <- list(sites,wSum_0_30,wSum_30_100)
      datos_ph <- join_all(dfs, "profileId")
      write.table(datos_ph, interpolado.archivo, row.names = F, sep=',')
    } else {
      cat(paste0('El archivo tabular de la interpolación de la variable pH existe, se va adicionar a la matriz','\n','\n'))
      datos_ph <- read.csv(interpolado.archivo)
    }
  }

  # Unir base de datos
  ##Carga de base con variables a modelar
  datos_modelos <- data.frame(read_excel(paste0(in.tb.data,'/originales/BD_OBSERVACIONES_MODELAMIENTO_PT_2020_09092020v1.xlsx'), sheet = 'Hoja1', na = "N/A"))

  if ('pH' %in% project.vars.all){
    ##renombrar columna
    names(datos_ph)[which(names(datos_ph) == 'profileId')] = 'COD_PERFIL'
    ##Union de valores de pH a base de variables taxon�micas y coordenadas
    datos_final <- plyr::join(datos_modelos,datos_ph,by='COD_PERFIL')
  } else{
    datos_final <- datos_modelos
  }

  # Factorizar columnas
  columnas_factores <- c('EPIPEDON', 'ENDOPEDON', 'ORDEN', 'SUBORDEN', 'SUBGRUPO', 'GRANGRUPO', 'FAMILIA_TE')
  datos_final <- datos_final %<>% mutate_at(columnas_factores, funs(factor(.)))

  datos_sp <-st_as_sf(points, coords=c("LONGITUD","LATITUD"))
  proyeccion <- CRS("+proj=longlat +datum=WGS84")
  st_crs(points) <- proyeccion
  datos_sp_cov <- st_transform(datos_sp, projection(dem))

  ###prueba areas pequeñas
  ## Cargar area limite
  #limite_shp <- st_read(paste0(in.geo.data,'/vector/limite/prueba'))
  #if (dim(limite_shp)[1] > 1){
  #  limite_shp$id <- 0
  #  limite_shp <- limite_shp %>% group_by(id) %>% summarize()
  #}
  #aoi <- st_transform(limite_shp, projection(dem))
  #out_cov <- st_intersection(datos_sp_cov, aoi)
  #out_cov <- as(out_cov, "Spatial")

  # Extraer valores metodo normal raster
  print(dim(cov))
  start <- Sys.time()
  #matriz_datos <- data.frame(raster::extract(covariables, out_cov))
  matriz_datos <- cbind(datos_final, raster::extract(covariables, datos_sp_cov))
  print(Sys.time() - start)

  # Exportar Matriz de Datos
  write.table(matriz_datos, paste0(out.tb.data,'/','MatrixDatos.csv'), row.names = F, sep=',')

  # Exportar GeoTIFF covariables
  writeRaster(covariables,paste0(out.geo.data,'/','covariables.tif'), drivers = 'GeoTIFF', overwrite=TRUE)
  saveRDS(names(covariables),paste0(out.geo.data,'/','covariables.rds'))

}