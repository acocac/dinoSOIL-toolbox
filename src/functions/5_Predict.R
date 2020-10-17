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

PredictGeoTIFF = function(COV, fit.model, out.file)
{ 
  # Evaluar el modelo
  #xtab <- table(fit.model$pred[,1], fit.model$pred[,2])
  #con.mat <- confusionMatrix(xtab)

  # Predecir en subarea
  #COVSub <- crop(COV, extent(COV, 1, 100, 1, 100))
  #pred <- predict(COVSub, fit.model, filename = paste0(out.data,'/',modelo,'_5m_PRED_orden_18092018_v2.individuales'),
  #              format = "GTiff", overwrite = T)
  no_cores <- detectCores() - 1
  beginCluster(no_cores)
  clusterR(COV, predict, args = list(fit.model),
      filename = out.file, format = "GTiff", progress = FALSE,
      overwrite = T)
  endCluster()

  #pred <- predict(COV, fit.model, filename = paste0(out.data,'/',modelo,'_PRED_',var,'.individuales'),
  #              format = "GTiff", overwrite = T)
}