#############################################################################
# titulo        : Configuracion modelos y calculo de importancia de las covariables;
# proposito     : Configurar modelos por DEFECTO o llamados por CONFIG;
# autor(es)     : Preparado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# creacion      : Creado por ACC en Bogota, Colombia en Septiembre 2020; Actualizado por ACC en Diciembre 2020;
# entrada       : N/A;
# salida        : Funciones;
# observaciones : Agregar a la funcion de importancia de las variables modelos que no tienen esa opcion en la libreria caret;
##############################################################################

dict <- new.env(hash = TRUE)
Add <- function(key, val) dict[[key]] <- val

modelos.config.manual <- function(){
  modelos.lista <- c('J48', 'C5.0', 'ranger', 'svmLinear','multinom',
                    'xgbTree', 'gbm_h2o', 'glmnet', 'mlp', 'svmRadial','cubist')
  
  modelos.dict = mapply(Add, modelos.lista, modelos.lista)

  #opcion defecto: todos con un mismo tamano para busqueda de mejores hiperparametros
  tuneLenght_size <- rep(20, length(modelos.lista))
  tuneLenght = mapply(Add, modelos.lista, tuneLenght_size)
  
  ##opcion alternativa: tamano para busqueda de mejores hiperparametros por modelo
  # tuneLenght <- c('J48'=5, 'C5.0'=5, 'multinom','ranger'=20, 'svmLinear'=5, 'xgbTree'=20, 'gbm_h2o'=3,
  #                'glmnet'=5,'mlp'=5, 'svmRadial'=20)
  
  conflist = list(modelos.dict, tuneLenght)
  names(conflist) = c('modelos.dict','tuneLenght')

  return (conflist)
  
}

modelos.config.defecto <- function(){
  ##list models
  regression.models <- map(getModelInfo(), "type") %>% 
    map_lgl(function(x) {
      any(x == "Regression")
    }) %>% 
    {.[.]} %>% 
    names() %>% 
    sort()
  
  clasification.models <- map(getModelInfo(), "type") %>% 
    map_lgl(function(x) {
      any(x == "Classification")
    }) %>% 
    {.[.]} %>% 
    names() %>% 
    sort()
  
  listmodels.varImp <- as.character(methods(varImp))
  listmodels.varImp <- gsub('^varImp.','', listmodels.varImp)
  listmodels.varImp <- listmodels.varImp[!listmodels.varImp %in% c('bagEarth','bagFDA','earth',
                                                                   'fda','gam','gbm','glm','JRip',
                                                                   'PART','rpart','nnet','avNNet')]
  
  proyecto.modelos.categoricas <- clasification.models[which(clasification.models %in% listmodels.varImp)]
  proyecto.modelos.continuas <- regression.models[which(regression.models %in% listmodels.varImp)]
  
  proyecto.modelos.categoricas <- c(proyecto.modelos.categoricas,'ranger')
  proyecto.modelos.continuas <- c(proyecto.modelos.continuas,'ranger')
  
  dict <- new.env(hash = TRUE)
  Add <- function(key, val) dict[[key]] <- val
  
  proyecto.modelos <- unique(c(proyecto.modelos.categoricas, proyecto.modelos.continuas))
  tuneLenght_size <- rep(20, length(proyecto.modelos))
  
  tuneLenght = mapply(Add, proyecto.modelos, tuneLenght_size)
  
  conflist = list(proyecto.modelos.continuas, proyecto.modelos.categoricas, tuneLenght)

  names(conflist) = c('modelos.continuas','modelos.categoricas','tuneLenght')

  return (conflist)
}

modelos.variables.importancia <- function(modelo,nombre){
  if (nombre == 'ranger'){
    
    finalModel <- modelo$finalModel

    #IMPORTANCIA VARIABLES
    imp <-as.vector(finalModel$variable.importance)
    variable <- names(finalModel$variable.importance)
    r <-data.frame(variable=variable,importance=imp)

    p <- ggplot(r, aes(x=reorder(variable,importance), y=importance,fill=importance))+ 
      geom_bar(stat="identity", position="dodge",fill = "darkgrey")+ coord_flip()+
      ylab("Importancia") +
      xlab("Covariable")+
      guides(fill=F) +
      theme_bw() +
      theme(text=element_text(size=18))
    
  } else{
    p <- NULL
  } 
  
  return (p)
}