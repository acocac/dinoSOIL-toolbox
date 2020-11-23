#############################################################################
# titulo        : Herramienta para explorar, procesar y analizar informacion de suelos
#                 bajo el enfoque de Mapeo Digital de Suelos (MDS);
# proposito     : Automatizar procesos relacionados con MDS dentro de la Subdireccion de Agrologia;
# autor(es)     : Conceptualizado y liderado por Alejandro Coca-Castro, CIAF/IGAC (PhD en Geografia, experto en Geotecnologias); Equipo de trabajo Oficina CIAF/IGAC: Pedro Karin Serrato (Profesor, experto en Agrologia),
#                 Victoria Camacho (Estadistica), Patricia Escudero (Estadistica), Andres Rosso (Candidato PhD en Ingenieria, experto en Machine Learning), Yesenia Vargas (MSc Geografia, experta en Proyectos);
#                 Equipo de trabajo Subdireccion de Agrologia/IGAC: Juan Camilo Garcia, Ricardo Devia Cartagena, Joan Sebastian Gutierrez Diaz, Daniela Prieto
#                 Supervisores: CIAF/IGAC, Carlos Franco y Diana Galindo - Subdireccion de Agrologia/IGAC, Janeth Gonzalez Nivia y Napoleon Ordoñz
# actualizacion : v1 Bogota, Colombia - Septiembre 2020;
# observaciones : Detalles de los pasos mencionados pueden consultarse en el manual;
#                 Los analisis deben correrse de manera separada;
#                 Esta es una herramienta en estado de prueba de concepto, en caso de encontrar errores
#                 favor reportar al correo acocac@gmail.com;
##############################################################################

# ------------------------------------------------------- #
# Limpiar espacio de trabajo y configurar opciones
# ------------------------------------------------------- #
rm(list = ls()); options(scipen = 999, warn = -1)

# ------------------------------------------------------- #
# Definir espacio y archivo de configuracion de trabajo
# ------------------------------------------------------- #
### Directorio de los codigos R  ### #copiar la ruta completa donde estan los codigos (carpeta src)
r.dir = 'F:/IDI_MDS_Agrologia/script/soil-toolbox/src'

### Directorio del proyecto ### #copiar la ruta completa donde se aloja el proyecto (carpeta proyecto)
# Indicar la ruta al proyecto
proyecto.dir = 'F:/IDI_MDS_Agrologia/script/soil-toolbox/proyecto_cesarmagdalena'
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
#para el demo llenar pregunta 1 OBSERVACIONES pregunta 2 BD_OBSERVACIONES_MODELAMIENTO_PT_2020.xlsx, pregunta 3 ORIGINAL_old, pregunta 4 COD_PERFIL
args_p1 <- prompt.user.part1()

#### ejecutar la funcion del modulo ###
Preprocesamiento(args_p1[[1]],args_p1[[2]],args_p1[[3]],args_p1[[4]])


########################################################
####PARTE 2 - Generacion de los Datos                ###
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/2_Datos.R'))

#### ejecutar la funcion del modulo ###
Datos()


########################################################
#####PARTE 3 - Seleccion variables                ######
########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/3_RFE.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 Variable Objetivo
args_p3 <- prompt.user.part3()

#### ejecutar la funcion del modulo ###
ExpRFE(args_p3[[1]], args_p3[[2]])


########################################################
#####PARTE 4a - Modelacion: Exploracion datos         ##
########################################################
##TODO: Boxplots y Variables Cualitativas
#### cargar modulo ###
source(paste0(r.dir,'/modules/4a_ModExploratorio.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 Variable Objetivo
args_p4a <- prompt.user.part4a()

#### ejecutar la funcion del modulo ###
ModExploracion(args_p4a[[1]], args_p4a[[2]], args_p4a[[3]])


########################################################
#####PARTE 4b - Modelacion: Ejecutar algoritmos     ####
########################################################
#TODO mejorar https://github.com/gimelbrantlab/magic/blob/0a86af1d18bb7b13a556090fa0bbe0fd5d424770/src/scores_ml.R
# https://github.com/CIAT-DAPA/sfs_project/blob/b679e418205054f0cce95cda19d0fb64f5c18eea/codes/01_2_SDrivers_process_input_data.R

##TODO: terminar entrenamiento por defecto o manual 
#### cargar modulo ###
source(paste0(r.dir,'/modules/4b_ModEjecutar.R'))

#### consultar al usuario argumentos del modulo ###
args_p4b <- prompt.user.part4b()

#### ejecutar la funcion del modulo ###
ModEntrenamiento(args_p4b[[1]],args_p4b[[2]],args_p4b[[3]],args_p4b[[4]],args_p4b[[5]])


###########################################################
#####PARTE 4c - Modelacion: Identificar mejor modelo    ###
###########################################################

#### cargar modulo ###
source(paste0(r.dir,'/modules/4c_ModMejor.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4c <- prompt.user.part4c()

#### ejecutar la funcion del modulo ###
ModMejorModelo(args_p4c[[1]],args_p4c[[2]],args_p4c[[3]], args_p4c[[4]], args_p4c[[5]])


###########################################################
#####PARTE 4d - Modelacion: Sensibilidad/Incertidumbre  ###
###########################################################
#TODO implementar incertidumbre metodo MGuevara Kriging https://github.com/marioguevara/utilityCodes/tree/e1fab9411f58138382734875c7be393e664fb055/chapter_5
#TODO implementar incertidumbre categoricas > https://github.com/samuel-rosa/qgis-r/blob/eb00ee202fc3c265acb3ae9cd3913c729837b463/rscripts/digital-soil-mapping.rsx
#### cargar modulo ###
source(paste0(r.dir,'/modules/4d_ModIncertidumbre.R'))

#### consultar al usuario argumentos del modulo ###
#para el demo llenar pregunta 1 C45
args_p4d <- prompt.user.part4d()

#### ejecutar la funcion del modulo ###
ModIncertidumbre(args_p4d[[1]],args_p4d[[2]],args_p4d[[3]], args_p4d[[4]], args_p4d[[5]])


###########################################################
#####PARTE 4e - Modelacion: Uso del modelo               ##
###########################################################
##TODO mejorar prediccion usando libreria terra combinando con funcion de prediccion como https://github.com/AusSoilsDSM/SLGA/blob/c407671f9b4c32bb86335098e8d8337c33e69746/SLGA/Production/NationalMaps/AustralianSoilClassification/Scripts/Miscellaneous/Darwin%20Area/ranger_categorical.R
#### cargar modulo ###
source(paste0(r.dir,'/modules/4e_ModUso.R')) ## TODO revisar predicion

#### consultar al usuario argumentos del modulo ###
args_p4e <- prompt.user.part4e()

#### ejecutar la funcion del modulo ###
ModUso(args_p4e[[1]], args_p4e[[2]], args_p4e[[3]], args_p4e[[4]], args_p4e[[5]])

###########################################################
#####                    FINAL                        #####
###########################################################

