#############################################################################
# titulo        : Herramienta para explorar, procesar y analizar informacion de suelos
#                 bajo el enfoque de Mapeo Digital de Suelos (MDS);
# proposito     : Automatizar procesos relacionados con MDS dentro de la Subdirección de Agrología;
# autor(es)     : Conceptualizado por Alejandro Coca-Castro, CIAF/IGAC (PhD Geografia, experto en Geotecnologias); Equipo de trabajo Oficina CIAF/IGAC: Pedro Karin Serrato (Profesor, experto en Agrologia),
#                 Victoria Camacho (Estadistica), Patricia Escudero (Estadistica), Andrés Rosso (PhD Ingenieria, experto en Machine Learning), Yesenia Vargas (MSc Geografia, experta en Proyectos);
#                 Equipo de trabajo Subdirección de Agrología/IGAC: Juan Camilo García, Ricardo Devia Cartagena, Joan Sebastián Gutiérrez Díaz
#                 Supervisores: CIAF/IGAC, Carlos Franco y Diana Galindo - Subdirección de Agrología/IGAC, Janeth González Nivia y Napoleón Ordoñez
# actualizacion : v1 Bogota, Colombia - Septiembre 2020;
# observaciones : Detalles de los pasos mencionados pueden consultarse en el manual;
#                 Los análisis deben correrse de manera separada;
#                 Esta es una herramienta en estado de prueba de concepto, en caso de encontrar errores
#                 favor reportar al correo acocac@gmail.com;
##############################################################################

# ------------------------------------------------------- #
# Limpiar espacio de trabajo
# ------------------------------------------------------- #
rm(list = ls())

# ------------------------------------------------------- #
# Definir espacio y archivo de configuracion de trabajo
# ------------------------------------------------------- #
### Directorio de los codigos R  ### #copiar la ruta completa donde estan los codigos (carpeta src)
r.dir = '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/src'

### Directorio del proyecto ### #copiar la ruta completa donde se aloja el proyecto (carpeta proyecto)
# Indicar la ruta al proyecto
proyecto.dir = '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena'
# Cargar el archivo de configuracion
conf.file = paste0(proyecto.dir,'/config/conf.txt')
# Cargar funciones de interaccion con el usuario
source(paste0(r.dir,'/functions/0a_UserArgs.R'))

########################################################
####PARTE 1 - Preprocesamiento de la base de datos #####
########################################################
##TODO falta verticalizacion de las observaciones
#### cargar modulo ###
source(paste0(r.dir,'/modules/1_Preprocesamiento.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 OBSERVACIONES pregunta 2 BD_OBSERVACIONES_MODELAMIENTO_PT_2020.xlsx, pregunta 3 ORIGINAL, pregunta 4 COD_PERFIL
args_p1 <- prompt.user.part1()

#### ejecutar la funcion del modulo ###
Preprocesamiento(args_p1[[1]],args_p1[[2]],args_p1[[3]],args_p1[[4]])


########################################################
####PARTE 2 - Generación de los Datos                ###
########################################################
##TODO
#### cargar modulo ###
source(paste0(r.dir,'/modules/2_Datos.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 OBSERVACIONES pregunta 2 BD_OBSERVACIONES_MODELAMIENTO_PT_2020.xlsx, pregunta 3 ORIGINAL, pregunta 4 COD_PERFIL
#args_p2 <- prompt.user.part2()

#### ejecutar la funcion del modulo ###
Datos()

########################################################
#####PARTE 3a - Analisis Exploratorio de los Datos ######
########################################################
##TODO

########################################################
#####PARTE 3b - Explotarorio RFE                  ######
########################################################
##TODO Done
#### cargar modulo ###
source(paste0(r.dir,'/modules/3b_RFE.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 Variable Objetivo
args_p3b <- prompt.user.part3b()

#### ejecutar la funcion del modulo ###
ExpRFE(args_p3b[[1]])

########################################################
#####PARTE 4a - Modelacion ML: Ejecutar algoritmos  ####
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/4a_ModEjecutar.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4a <- prompt.user.part4a()

#### ejecutar la funcion del modulo ###
ModEntrenamiento(args_p4a[[1]])


###########################################################
#####PARTE 4b - Modelacion ML: Identificar mejor modelo  ##
###########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/4b_ModMejor.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4b <- prompt.user.part4b()

#### ejecutar la funcion del modulo ###
ModMejorModelo(args_p4b[[1]])


###########################################################
#####PARTE 4c - Modelacion ML: Sensibilidad/Incertidumbre #
###########################################################
##TODO


###########################################################
#####PARTE 4d - Modelacion: Uso del modelo               ##
###########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/4d_ModUso.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4d <- prompt.user.part4d()

#### ejecutar la funcion del modulo ###
ModUso(args_p4d[[1]])

###########################################################
#####                    FINAL                        #####
###########################################################