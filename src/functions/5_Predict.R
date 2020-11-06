##############################################################################
# title         : grid files (inputs) to extract fragstat and fractal metrics;
# purpose       : create separated grids (unit of analysis) from detection grid for fragstat and 
#                 fractal analyses;
# producer      : prepared by A. Coca;
# last update   : in London, UK June 2015 / Updated in September 2015;
# inputs        : deforestation grid by year, fishnet (windows) shapefile;
# outputs       : split detection grid using  in GeoTIFF format (FRAGSTAT/FRACTAL INPUT);
# remarks 1     : detection grid must be in projected projection (i.e IGH or LAE);
###############################################################################

# Funtion to compute the Shannon entropy ----
entropy <-
  function (x) {
    - sum(x * log(x, base = length(x)), na.rm = TRUE)
  }

# Function to compute the confusion index ----
confusion <-
  function (x) {
    1 - diff(sort(x, decreasing = TRUE)[2:1])
  }

PredictGeoTIFF = function(COV, fit.model, out.file, type, index, train.data)
{ 
  # Evaluar el modelo
  #xtab <- table(fit.model$pred[,1], fit.model$pred[,2])
  #con.mat <- confusionMatrix(xtab)
  #
  #limite <- st_read('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/1_covariables/vector/limite/prueba')
  #if (dim(limite)[1] > 1){
  #    limite$id <- 0
  #    limite <- limite %>% group_by(id) %>% summarize()
  #}
  #
  #covariates <- raster::crop(COV, limite)
  #writeRaster(covariates, '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/temp.dir/cov_sample.tif', options=c("COMPRESS=DEFLATE", "TFW=YES"), overwrite=TRUE)
  #
  #cov = stack('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/temp.dir/cov_sample.tif')
  #names(cov) <- names(COV)
  #COV <- cov

  ## Make spatial predictions ----
  #no_cores <- detectCores() - 1
  #raster::beginCluster(no_cores)
  #prediction <-
  #  raster::clusterR(
  #    raster::stack(COV),
  #    raster::predict,
  #    args = list(model = fit.model, type = type, index = index)
  #  )
  #raster::endCluster()
  #TODO parallel https://strimas.com/post/processing-large-rasters-in-r/
  if (type == "prob") {
    data_type = 'FLT4S'
  } else{
    data_type = 'INT1U'
  }

  no_cores <- detectCores() - 1
  beginCluster(no_cores)
  clusterR(COV, predict, args = list(fit.model, type = type, index = index),
      filename = out.file, format = "GTiff",
      #overwrite = T, options=c("COMPRESS=DEFLATE", "TFW=YES"))
      overwrite = T, datatype=data_type, options='COMPRESS=YES')
  endCluster()
  #
  #if (type == "prob") {
  #  #prob.file <- gsub('_PRED_','_PROB_',out.file)
  #  #incertidumbre.file <- gsub('_PRED_','_INCERTIDUMBRE_',out.file)
  #  #
  #  #Predictions <- as.factor(calc(x = prediction, fun = nnet::which.is.max))
  #  #Predictions <- ratify(Predictions)
  #  #rat <- levels(Predictions)[[1]]
  #  #rat$class <- levels(train.data[['target']])[rat$ID]
  #  #levels(Predictions) <- rat
  #  #
  #  #print('Atributos del raster creado')
  #  #
  #  #Uncertainty <-
  #  #brick(
  #  #  calc(x = prediction, fun = entropy),
  #  #  calc(x = prediction, fun = confusion)
  #  #)
  #  #
  #  #print('Capas de incertidumbre generadas')
  #
  #  writeRaster(prediction, prob.file, format = "GTiff", overwrite = T, options=c("COMPRESS=DEFLATE", "TFW=YES"))
  #  #writeRaster(Predictions, out.file, format = "GTiff", datatype="INT1U", RAT=TRUE, overwrite = T, options=c("COMPRESS=DEFLATE", "TFW=YES"))
  #  #writeRaster(Uncertainty, incertidumbre.file, format = "GTiff", overwrite = T, options=c("COMPRESS=DEFLATE", "TFW=YES"))
  #  #write.csv(rat, file=gsub('.tif$','.csv',out.file), row.names = F)
  #}
  #else if (type == "raw") {
  #  writeRaster(prediction, out.file, format = "GTiff", overwrite = T, options=c("COMPRESS=DEFLATE", "TFW=YES"))
  #}
    #
  #no_cores <- detectCores() - 1
  #beginCluster(no_cores)
  #clusterR(COV, predict, args = list(fit.model, type = type, index = index),
  #    filename = out.file, format = "GTiff",
  #    overwrite = T, options=c("COMPRESS=DEFLATE", "TFW=YES"))
  #endCluster()

  #pred <- predict(COV, fit.model, filename = paste0(out.data,'/',modelo,'_PRED_',var,'.individuales'),
  #              format = "GTiff", overwrite = T)

}