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
r.dir = '/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/src'

### Directorio del proyecto ### #copiar la ruta completa donde se aloja el proyecto (carpeta proyecto)
# Indicar la ruta al proyecto
proyecto.dir = '/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_sibundoy'
# Cargar el archivo de configuracion
conf.file = paste0(proyecto.dir,'/config/conf.txt')
# Cargar funciones de interaccion con el usuario
source(paste0(r.dir,'/functions/0a_UserArgs.R'))

########################################################
####PARTE 1 - Preprocesamiento de la base de datos #####
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/1_Preprocesamiento.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 OBSERVACIONES_20160603_VF.xls, pregunta 2 RESHAPE, pregunta 3 Obs
args_p1 <- prompt.user.part1()

#### ejecutar la funcion del modulo ###
Preprocesamiento(args_p1[[1]],args_p1[[2]],args_p1[[3]])


########################################################
####PARTE 2 - Preprocesamiento de las covariables ######
########################################################


########################################################
#####PARTE 3 - Analisis Exploratorio de los Datos ######
########################################################


########################################################
#####PARTE 4 - Crear datos de entrenamiento     ########
########################################################


########################################################
#####PARTE 5a - Modelacion: Ejecutar algoritmos     ####
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/5a_ModEjecutar.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
#args_p2 <- prompt.user.part2()

#### ejecutar la funcion del modulo ###
ModEntrenamiento()

########################################################
#####PARTE 5b - Modelacion: Identificar mejor resultado#
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/5b_ModMejor.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
#args_p2 <- prompt.user.part2()

#### ejecutar la funcion del modulo ###
ModMejorModelo()

########################################################
#####PARTE 5c - Modelacion: Sensibilidad/Incertidumbre #
########################################################


########################################################
#####PARTE 5d - Modelacion: Uso del modelo          ####
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/5d_ModUso.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
#args_p2 <- prompt.user.part2()

#### ejecutar la funcion del modulo ###
ModUso()