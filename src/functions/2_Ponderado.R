#############################################################################
# titulo        : Datos de entrada (matriz) y covariables ambientales (GeoTIFF);
# proposito     : Generar datos de entrada (matriz) y raster multibanda con las covariables ambientales (GeoTIFF);
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogotá, Colombia / Actualizado por ACC en Octubre 2020;;
# entrada       : Base de datos verticalizada;
# salida        : Datos de entrada y GeoTIFF con las covariables ambientales para su uso en la predicción;
# observaciones : ninguna;
##############################################################################

Ponderado = function(data2, tarvar, upDepth, lowDepth)
{
  # Nuevos rangos de prof inicial y final para cada horizonte
  data2$upper <- ifelse(data2$top <= upDepth, yes = upDepth, no = data2$top)
  data2$upper <- ifelse(data2$top >= lowDepth, yes = lowDepth, no = data2$upper)
  data2$lower <- ifelse(data2$bottom >= lowDepth, yes = lowDepth, no = data2$bottom)
  data2$lower <- ifelse(data2$bottom < upDepth, yes = upDepth, no = data2$lower)

  # Nuevo espesor de cada horizonte
  data2$depth <- data2$lower - data2$upper

  # Descartar horizonte sin dato de pH
  (no.inf.indx <- which(is.na(data2[,tarvar])))
  data2 <- data2[-no.inf.indx,]

  # Descartar horizontes que no caen en el rango deseado
  data2 <- data2[data2$depth!=0,]

  # Ponderacion de valores de pH por horizonte
  data2$weighted <- (data2$depth*data2[,tarvar])/(lowDepth-upDepth)

  # Suma ponderada de valores de pH por perfil
  wSum<- ddply(
    .data = data2,
    .variables = "profileId",
    .fun = function(x){
      sum(x$weighted)
    }
  )

  # Resultado final
  colnames(wSum)[2] <- paste0(tarvar,".",upDepth,"_",lowDepth,"_Sum.Pond")
  wSum$profileId <- as.character(wSum$profileId)
  return(wSum)
}