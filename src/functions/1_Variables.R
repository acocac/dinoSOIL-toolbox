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

VariablesObjectivo = function()
{
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  project.folder <- conf.args[[1]]
  project.name <- sapply(strsplit(project.folder, '_'), tail, 1)
  project.vars.categoricas <- conf.args[[2]]
  project.vars.categoricas = unlist(strsplit(project.vars.categoricas,';'))
  project.vars.continuas <- conf.args[[3]]
  project.vars.continuas = unlist(strsplit(project.vars.continuas,';'))

  carpeta.entrada <- paste0(project.folder,'/datos/salida/0_matriz')
  datos.entrada <- read.csv(paste0(carpeta.entrada,'/MatrixDatos.csv'),sep=',')
  
  project.vars.categoricas <- paste(project.vars.categoricas, collapse='|')
  project.vars.continuas <- paste(project.vars.continuas, collapse='|')

  #columnas.matriz <- names(datos.entrada[which(grepl(project.vars.categoricas,colnames(datos.entrada)) |  startsWith(colnames(datos.entrada),project.vars.continuas))])
  columnas.matriz <- names(datos.entrada[which(grepl(project.vars.categoricas,colnames(datos.entrada)) |  grepl(project.vars.continuas,colnames(datos.entrada)))])
  
  return(columnas.matriz)
}
