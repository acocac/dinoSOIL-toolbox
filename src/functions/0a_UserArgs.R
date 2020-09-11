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
  message(prompt="Indique el nombre del archivo que contiene la base de datos horizontal:>>> ")
  a <- readLines(n = 1)
  a <- gsub("\\\\", "/", a)

  message(prompt="Indique el nombre de la pestana del archivo que contiene la base de datos:>>> ")
  b <- readLines(n = 1)
  b <- gsub("\\\\", "/", b)

  message(prompt="Indique el nombre de la columna que tiene el ID de los perfiles:>>> ")
  c <- readLines(n = 1)
  c <- gsub("\\\\", "/", c)

  newlist = list(a,b,c)
  return(newlist)
}