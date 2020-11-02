#############################################################################
# titulo        : Herramienta para explorar, procesar y analizar informacion de suelos
#                 bajo el enfoque de Mapeo Digital de Suelos (MDS);
# proposito     : Automatizar procesos relacionados con MDS dentro de la Subdirección de Agrología;
# autor(es)     : Conceptualizado y liderado por Alejandro Coca-Castro, CIAF/IGAC (PhD en Geografía, experto en Geotecnologias); Equipo de trabajo Oficina CIAF/IGAC: Pedro Karin Serrato (Profesor, experto en Agrologia),
#                 Victoria Camacho (Estadistica), Patricia Escudero (Estadistica), Andrés Rosso (Candidato PhD en Ingeniería, experto en Machine Learning), Yesenia Vargas (MSc Geografia, experta en Proyectos);
#                 Equipo de trabajo Subdirección de Agrología/IGAC: Juan Camilo García, Ricardo Devia Cartagena, Joan Sebastián Gutiérrez Díaz, Daniela Prieto
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

#### cargar modulo ###
source(paste0(r.dir,'/modules/2_Datos.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 OBSERVACIONES pregunta 2 BD_OBSERVACIONES_MODELAMIENTO_PT_2020.xlsx, pregunta 3 ORIGINAL, pregunta 4 COD_PERFIL
#args_p2 <- prompt.user.part2()

#### ejecutar la funcion del modulo ###
Datos()


########################################################
#####PARTE 3 - Selección variables                ######
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/3_RFE.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 Variable Objetivo
args_p3 <- prompt.user.part3()

#### ejecutar la funcion del modulo ###
ExpRFE(args_p3[[1]])


########################################################
#####PARTE 4a - Modelación: Exploración datos         ##
########################################################
##TODO: Boxplots y Variables Cualitativas
#### cargar modulo ###
source(paste0(r.dir,'/modules/4a_ModExploratorio.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 Variable Objetivo
args_p4a <- prompt.user.part4a()

#### ejecutar la funcion del modulo ###
ModExploracion(args_p4a[[1]], args_p4a[[2]])


########################################################
#####PARTE 4b - Modelación: Ejecutar algoritmos     ####

#### cargar modulo ###
source(paste0(r.dir,'/modules/4b_ModEjecutar.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4b <- prompt.user.part4b()

#### ejecutar la funcion del modulo ###
ModEntrenamiento(args_p4b[[1]],args_p4b[[2]])


###########################################################
#####PARTE 4c - Modelación: Identificar mejor modelo    ###
###########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/4c_ModMejor.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4c <- prompt.user.part4c()

#### ejecutar la funcion del modulo ###
ModMejorModelo(args_p4c[[1]],args_p4c[[2]])


###########################################################
#####PARTE 4d - Modelación: Sensibilidad/Incertidumbre  ###
###########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/4d_ModIncertidumbre.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4d <- prompt.user.part4d()

#### ejecutar la funcion del modulo ###
ModIncertidumbre(args_p4d[[1]],args_p4d[[2]])


###########################################################
#####PARTE 4e - Modelacion: Uso del modelo               ##
###########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/4e_ModUso.R')) ## TODO revisar predicion

#### consultar al usuario argumentos del modulo ###
args_p4e <- prompt.user.part4e()

#### ejecutar la funcion del modulo ###
ModUso(args_p4e[[1]],args_p4e[[2]])

###########################################################
#####                    FINAL                        #####
###########################################################