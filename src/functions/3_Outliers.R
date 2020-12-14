#############################################################################
# titulo        : Datos atipicos;
# proposito     : Identificar datos atipicos;
# autor(es)     : Preparado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# creacion      : Creado por ACC en Bogota, Colombia en Septiembre 2020; Actualizado por ACC en Diciembre 2020;
# entrada       : Matriz de datos de entrada de la variable objetivo;
# salida        : Matriz sin datos atipicos de la variable objetivo;
# observaciones : ninguna;
##############################################################################

#Perform recursive feature elimination
findOutliers <- function(col,coef){
  cuartil.primero = quantile(col,0.05, na.rm=TRUE)
  cuartil.tercero = quantile(col,0.95, na.rm=TRUE)
  iqr <- cuartil.tercero - cuartil.primero

  extremo.superior.outlier <- cuartil.tercero + coef * iqr
  extremo.inferior.outlier <- cuartil.primero - coef * iqr

  return( which((col > extremo.superior.outlier) | (col < extremo.inferior.outlier)))
}

vector_claves_outliers_IQR_en_alguna_columna <- function(datos, coef=1.5){
  vector.es.outlier <- sapply(datos[1:ncol(datos)], findOutliers,coef)
  vector.es.outlier
}

computeOutliers <- function(data, type='remove', k=2, coef = 1.5){
  outliers <- vector_claves_outliers_IQR_en_alguna_columna(data, coef)
  if (type == 'remove'){
    index.to.keep <- setdiff(c(1:nrow(data)),unlist(outliers))
    return (index.to.keep)
  }
  else if(type == 'knn'){
    data.with.na <- changeOutliersValue(outliers,data, type='knn')
    return(computeMissingValues(data.with.na,type='knn',k=k))
  }
  else if(type == 'median'){
    return(changeOutliersValue(outliers,data))
  }
  else if(type == 'mean'){
    return(changeOutliersValue(outliers,data, type = 'mean'))
  }
  else if(type == 'rf'){
    data.with.na <- changeOutliersValue(outliers,data, type='rf')
    return(computeMissingValues(data.with.na,type='rf'))
  }
  else if(type == 'mice'){
    data.with.na <- changeOutliersValue(outliers,data, type='mice')
    return(computeMissingValues(data,type='mice'))
  }

  return(data) # es necesario?
}

changeOutliersValue <- function(outliers,data,type = 'median'){
  i = 1
  j = 1

  n = ncol(data)

  while(j <= n){
    outliers_columna = outliers[[j]]
    m = length(outliers_columna)

    while(i <= m){
      if (type == 'median'){
        data[outliers_columna[i],j] = median(data[,j], na.rm = TRUE)
      }
      else if(type == 'mean'){
        data[outliers_columna[i],j] = mean(data[,j], na.rm = TRUE)
      }
      else {
        data[outliers_columna[i],j] = NA
      }

      i = i +1
    }

    i = 1
    j = j + 1
  }
  return(data)
}