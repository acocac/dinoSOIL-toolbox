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

PonderadopH = function(data2, upDepth, lowDepth)
{
  # Nuevos rangos de prof inicial y final para cada horizonte
  data2$upper <- ifelse(data2$top <= upDepth, yes = upDepth, no = data2$top)
  data2$upper <- ifelse(data2$top >= lowDepth, yes = lowDepth, no = data2$upper)
  data2$lower <- ifelse(data2$bottom >= lowDepth, yes = lowDepth, no = data2$bottom)
  data2$lower <- ifelse(data2$bottom < upDepth, yes = upDepth, no = data2$lower)

  # Nuevo espesor de cada horizonte
  data2$depth <- data2$lower - data2$upper

  # Descartar horizonte sin dato de pH
  (no.inf.indx <- which(is.na(data2$pH)))
  data2 <- data2[-no.inf.indx,]

  # Descartar horizontes que no caen en el rango deseado
  data2 <- data2[data2$depth!=0,]

  # Ponderacion de valores de pH por horizonte
  data2$weighted <- (data2$depth*data2$pH)/(lowDepth-upDepth)

  # Suma ponderada de valores de pH por perfil
  wSum<- ddply(
    .data = data2,
    .variables = c("profileId"),
    .fun = function(x){
      sum(x$weighted)
    }
  )

  # Resultado final
  colnames(wSum)[2] <- paste0("pH.",upDepth,"_",lowDepth,"_Sum.Pond")
  wSum$profileId <- as.character(wSum$profileId)
  return(wSum)
}
