#############################################################################
# titulo        : Ponderado por pesos;
# proposito     : Generar valores ponderados por peso;
# autor(es)     : Preparado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# creacion      : Creado por ACC en Bogota, Colombia en Septiembre 2020; Actualizado por ACC en Diciembre 2020;
# entrada       : Base de datos verticalizada de la variable objetivo continua;
# salida        : Interpolación de profundidades de la variable continua;
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
