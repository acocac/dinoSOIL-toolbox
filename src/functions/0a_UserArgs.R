##############################################################################
# title         : user dirs prompt;
# purpose       : determinar argumentos del parte 1;
# producer      : prepared by A. Coca-Castro;
# last update   : in Bogota, Colombia - Septiembre 2020;
# inputs        : NA;
# outputs       : archivos objetivo;
# remarks 1     : ;
###############################################################################

prompt.user.part1 <- function()#get arguments from user
{

  message(prompt="Indique tipo de base de datos para verticalizar (PERFILES o OBSERVACIONES):>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el nombre del archivo que contiene la base de datos horizontal:>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  message(prompt="Indique el nombre de la pestana del archivo que contiene la base de datos:>>> ")
  c <- readLines(n = 1)
  c <- gsub("\\\\", "/", c)

  message(prompt="Indique el nombre de la columna que tiene el ID de los perfiles:>>> ")
  d <- readLines(n = 1)
  d <- gsub("\\\\", "/", d)

  newlist = list(a,b,c,d)
  return(newlist)
}

prompt.user.part2 <- function()#get arguments from user
{
  message(prompt="Indique el nombre del modelol:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  newlist = list(a)
  return(newlist)
}

prompt.user.part3b <- function()#get arguments from user
{
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  variables.usuario <- VariablesObjectivo()
  cat(paste0('Las siguientes columnas estan disponibles para su modelación:','\n'))
  cat(paste0(variables.usuario, sep=" | "))
  cat(paste0('\n','\n'))
  message(prompt="Indique el nombre de la variable objetivo de acuerdo al listado superior:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  newlist = list(a)
  return(newlist)
}

prompt.user.part4a <- function()#get arguments from user
{
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  variables.usuario <- VariablesObjectivo()
  cat(paste0('Las siguientes columnas estan disponibles para su modelación:','\n'))
  cat(paste0(variables.usuario, sep=" | "))
  cat(paste0('\n','\n'))

  message(prompt="Indique el nombre de la variable objetivo de acuerdo al listado superior:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el numero limite de covariables a considerar según interpretación del RFE y Boruta:>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  newlist = list(a, b)
  return(newlist)
}

prompt.user.part4b <- function()#get arguments from user
{
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  variables.usuario <- VariablesObjectivo()
  cat(paste0('Las siguientes columnas estan disponibles para su modelación:','\n'))
  cat(paste0(variables.usuario, sep=" | "))
  cat(paste0('\n','\n'))
  message(prompt="Indique el nombre de la variable objetivo de acuerdo al listado superior:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  newlist = list(a)
  return(newlist)
}

prompt.user.part4d <- function()#get arguments from user
{
  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/1_Variables.R'))

  variables.usuario <- VariablesObjectivo()
  cat(paste0('Las siguientes columnas estan disponibles para su modelación:','\n'))
  cat(paste0(variables.usuario, sep=" | "))
  cat(paste0('\n','\n'))
  message(prompt="Indique el nombre de la variable objetivo de acuerdo al listado superior:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  newlist = list(a)
  return(newlist)
}