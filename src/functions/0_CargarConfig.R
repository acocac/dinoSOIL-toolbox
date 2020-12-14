#############################################################################
# titulo        : Datos del archivo config.txt;
# proposito     : Cargar los datos del archivo config.txt;
# autor(es)     : Preparado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# creacion      : Creado por ACC en Bogota, Colombia en Septiembre 2020; Actualizado por ACC en Diciembre 2020;
# entrada       : config.txt;
# salida        : listado de variables para su uso en la herramienta;
# observaciones : ninguna;
##############################################################################

LoadConfig = function(x){ 
  conf.list <- lapply(strsplit(readLines(x, warn=FALSE)," "), as.character)

  #read target lines
  root.index <- grep("*proyecto.dir",conf.list)
  root.path = conf.list[[root.index]][[length(conf.list[[root.index]])]]

  #leer variables objetivo categoricas
  variables.categoricas.index <- grep("*variables.categoricas",conf.list)
  root.vars.categoricas = conf.list[[variables.categoricas.index]][[length(conf.list[[variables.categoricas.index]])]]
  
  #leer variables objetivo continuas
  variables.continuas.index <- grep("*variables.continuas",conf.list)
  root.vars.continuas = conf.list[[variables.continuas.index]][[length(conf.list[[variables.continuas.index]])]]

  #leer ajustes variables objetivo categoricas
  ajuste.categoricas.index <- grep("*categoricas.minobservaciones",conf.list)
  root.ajuste.categoricas = conf.list[[ajuste.categoricas.index]][[length(conf.list[[ajuste.categoricas.index]])]]

  #leer ajustes variables objetivo continuas
  ajuste.continuas.index <- grep("*continuas.profundidades",conf.list)
  root.ajuste.continuas = conf.list[[ajuste.continuas.index]][[length(conf.list[[ajuste.continuas.index]])]]

  #leer lista covariables en archivos vector
  covariables.vector.index <- grep("*covariables.vector",conf.list)
  root.covariables.vector = conf.list[[covariables.vector.index]][[length(conf.list[[covariables.vector.index]])]]

  #leer lista covariables en archivos vector
  covariables.raster.index <- grep("*covariables.raster",conf.list)
  root.covariables.raster = conf.list[[covariables.raster.index]][[length(conf.list[[covariables.raster.index]])]]

  #leer atributos covariables en archivos vector
  covariables.atributos.index <- grep("*vector.atributos",conf.list)
  root.covariables.atributos = conf.list[[covariables.atributos.index]][[length(conf.list[[covariables.atributos.index]])]]

  #leer fechas ndvi
  fechas.ndvi.index <- grep("*NDVI.fechas",conf.list)
  root.fechas.ndvi = conf.list[[fechas.ndvi.index]][[length(conf.list[[fechas.ndvi.index]])]]
  
  #leer modelos segun variable
  modelos.categoricas.index <- grep("*modelos.categoricas",conf.list)
  root.modelos.categoricas = conf.list[[modelos.categoricas.index]][[length(conf.list[[modelos.categoricas.index]])]]
  
  modelos.continuas.index <- grep("*modelos.continuas",conf.list)
  root.modelos.continuas = conf.list[[modelos.continuas.index]][[length(conf.list[[modelos.continuas.index]])]]
  
  #leer metricas segun variable
  metricas.categoricas.index <- grep("*metricas.categoricas",conf.list)
  root.metricas.categoricas = conf.list[[metricas.categoricas.index]][[length(conf.list[[metricas.categoricas.index]])]]

  metricas.continuas.index <- grep("*metricas.continuas",conf.list)
  root.metricas.continuas = conf.list[[metricas.continuas.index]][[length(conf.list[[metricas.continuas.index]])]]

  #lista para exportar de la funcion
  newlist = list(root.path, root.vars.categoricas, root.vars.continuas, root.ajuste.categoricas, root.ajuste.continuas, root.covariables.vector, root.covariables.raster,
                 root.covariables.atributos, root.fechas.ndvi, root.modelos.categoricas, root.modelos.continuas, root.metricas.categoricas, root.metricas.continuas)

  names(newlist) = c('proyecto.carpeta', 'vars.categoricas', 'vars.continuas', 'ajuste.categoricas', 'ajuste.continuas', 'covariables.vector', 'covariables.raster',
                          'vector.atributos', 'fechas.ndvi', 'modelos.categoricas', 'modelos.continuas', 'metricas.categoricas', 'metricas.continuas')

  return(newlist)
}

LoadConfig(conf.file)