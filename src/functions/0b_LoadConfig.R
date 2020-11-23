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

LoadConfig = function(x){ 
  conf.list <- lapply(strsplit(readLines(x, warn=FALSE)," "), as.character)

  #read target lines
  root.index <- grep("*path",conf.list)
  root.path = conf.list[[root.index]][[length(conf.list[[root.index]])]]
  
  #leer variables objetivo categoricas
  variables.categoricas.index <- grep("*variables.categoricas",conf.list)
  root.vars.categoricas = conf.list[[variables.categoricas.index]][[length(conf.list[[variables.categoricas.index]])]]
  
  #leer variables objetivo continuas
  variables.continuas.index <- grep("*variables.continuas",conf.list)
  root.vars.continuas = conf.list[[variables.continuas.index]][[length(conf.list[[variables.continuas.index]])]]
  
  #leer lista covariables
  covariables.list.index <- grep("*covariables",conf.list)
  root.covariables.list = conf.list[[covariables.list.index]][[length(conf.list[[covariables.list.index]])]]
  
  #leer fechas ndvi
  fechas.ndvi.index <- grep("*NDVI.fechas",conf.list)
  root.fechas.ndvi = conf.list[[fechas.ndvi.index]][[length(conf.list[[fechas.ndvi.index]])]]
  
  #leer modelos segun variable
  modelos.categoricas.index <- grep("*modelos.categoricas",conf.list)
  root.modelos.categoricas = conf.list[[modelos.categoricas.index]][[length(conf.list[[modelos.categoricas.index]])]]
  
  modelos.continuas.index <- grep("*modelos.continuas",conf.list)
  root.modelos.continuas = conf.list[[modelos.continuas.index]][[length(conf.list[[modelos.continuas.index]])]]
  
  #leer métricas segun variable
  metricas.categoricas.index <- grep("*metricas.categoricas",conf.list)
  root.metricas.categoricas = conf.list[[metricas.categoricas.index]][[length(conf.list[[metricas.categoricas.index]])]]

  metricas.continuas.index <- grep("*metricas.continuas",conf.list)
  root.metricas.continuas = conf.list[[metricas.continuas.index]][[length(conf.list[[metricas.continuas.index]])]]
  
  #lista para exportar de la función
  newlist = list(root.path, root.vars.categoricas, root.vars.continuas, root.covariables.list, root.fechas.ndvi, 
                 root.modelos.categoricas, root.modelos.continuas, root.metricas.categoricas, root.metricas.continuas)
  
  return(newlist)
}

LoadConfig(conf.file)