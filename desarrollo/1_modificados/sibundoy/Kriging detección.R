# ############################################## ##########################
# titulo: Herramienta para explorar, procesar y analizar informacion de suelos
#                  bajo el enfoque de Mapeo Digital de Suelos (MDS);
# proposito: Automatizar procesos relacionados con MDS dentro de la Subdirecci√≥n de Agrolog√≠a;
# autor (es): Conceptualizado por Alejandro Coca-Castro, CIAF / IGAC (PhD Geografia, experto en Geotecnologias); Equipo de trabajo Oficina CIAF / IGAC: Pedro Karin Serrato (Profesor, experto en Agrologia),
#                  Victoria Camacho (Estad√≠stica), Patricia Escudero (Estad√≠stica), Andr√©s Rosso (PhD Ingenieria, experto en Machine Learning), Yesenia Vargas (MSc Geografia, experta en Proyectos);
#                  Equipo de trabajo Subdirecci√≥n de Agrolog√≠a / IGAC: Juan Camilo Garc√≠a, Ricardo Devia Cartagena, Joan Sebasti√°n Guti√©rrez D√≠az
#                  Supervisores: CIAF / IGAC, Carlos Franco y Diana Galindo - Subdirecci√≥n de Agrolog√≠a / IGAC, Janeth Gonz√°lez Nivia y Napole√≥n Ordo√±ez
# actualizaci√≥n: v1 Bogot√°, Colombia - Septiembre 2020;
# observaciones: Detalles de los pasos mencionados pueden consultar en el manual;
#                  Los an√°lisis deben correrse de manera separada;
#                  Esta es una herramienta en estado de prueba de concepto, en caso de encontrar errores
#                  favor reportar al correo acocac@gmail.com;
# ############################################## ###########################

# ------------------------------------------------- ------ #
# Limpiar espacio de trabajo
# ------------------------------------------------- ------ #
rm ( lista  = ls ())

# ------------------------------------------------- ------ #
# Definir espacio y archivo de configuraci√≥n de trabajo
# ------------------------------------------------- ------ #
# ## Directorio de los codigos R ### #copiar la ruta completa donde estan los codigos (carpeta src)
r.dir  =  ' / Usuarios / ac / Documentos / Consultor√≠a / IGAC / proyectos / 3_mapeosuelos / desarrollos / suelo-toolbox / src '

# ## Directorio del proyecto ### #copiar la ruta completa donde se aloja el proyecto (carpeta proyecto)
# Indicar la ruta al proyecto
proyecto.dir  =  ' / Usuarios / ac / Documentos / Consultor√≠a / IGAC / proyectos / 3_mapeosuelos / desarrollos / suelo-toolbox / proyecto_sibundoy '
# Cargar el archivo de configuracion
conf.file  = paste0 ( proyecto.dir , ' /config/conf.txt ' )
# Cargar funciones de interaccion con el usuario
fuente (paste0 ( r.dir , ' /functions/0a_UserArgs.R ' ))

# ############################################## ######
# ### PARTE 1 - Preprocesamiento de la base de datos #####
# ############################################## ######

# ### cargar modulo ###
fuente (paste0 ( r.dir , ' /modules/1_Preprocesamiento.R ' ))

# ### consultar al usuario argumentos del modulo ###
# para el demo llenar pregunta 1 OBSERVACIONES_20160603_VF.xls, pregunta 2 RESHAPE, pregunta 3 Obs
args_p1  <- prompt.user.part1 ()

# ### ejecutar la funcion del modulo ###
Preprocesamiento ( args_p1 [[ 1 ]], args_p1 [[ 2 ]], args_p1 [[ 3 ]])








#Script kriging#

## Se mira cual es el directorio de trabajo 
getwd()


#mirar que funciones se necesitan
miFunction <- function(datos){
  if (!require('paqueteNecesario'))
    stop("Debe instalar el paquete 'paqueteNecesario' para continuar")
  c√≥digo-de-la-funci√≥n
}

usePackage  <-  funci√≥n ( p ) {
  if ( ! is.element ( p , installed.packages () [, 1 ]))
    install.packages ( p , dep  =  TRUE )
  require ( p , character.only  =  TRUE )
}

#VERTICALIZAR LA BASE FALTA

lapply ( pckg , usePackage )
getSymbols.yahoo
## cargar paquetes necesarios y descripci√≥n
library(sp)#clases y m√©todos para datos espaciales
library(raster)#introducci√≥n a datos raster en R(es necesario el paquete sp)
library(car)#regresion aplicada
library(rgdal)#enlaces para la bilbioteca de abstracci√≥n de datos"geoespacial"
library(gstat)#modelamiento, predicci√≥n y simulaci√≥n geoestad√≠stica espacial y espacio temporal
              #modelamiento variograma
library(lattice)#visualizaci√≥n de datos de alto nivel. Necesidades gr√°ficas
library(ggplot2)#costrucci√≥n de gr√°ficos
library(caret)#funciones que facilitan el uso m√©odos complejos de clasificaci√≥n y regresi√≥n
library(reshape)#transformaciones de datos, funciones melt y cast (reorganizar y resumir datos)
library(aplpack)#dibujar gr√°ficos especiales
library(tcltk)#interfaz que vincula y usa lenguaje vinculado a elementos TCL/TKGUI
library(MASS)#funciones de soporte y conjunto de datos
require(MASS)#Retorna FALSE y emite un warning en caso de no poder cargar el paquete. No detiene la eventual ejecuci√≥n del script. 
library(akima)#interpolaci√≥n de datos espaciados de forma regular e irregular
library(geoR)#herramientas para el an√°lisis de datos geoestad√≠sticos
library(lattice)#repetido 
library(maptools)#conjunto de herramientas para manipular datos geogr√°ficos

### Se preparan los datos ####
dir()#enumera todos los archivos de un directorio

# Se Cargan los datos de los splines 
dat <- read.csv("RegMatrix_VF.csv")#lee los datos
names(dat)#nombre de las variables

#Cargar base de excel
library(readxl)
dat<- read_excel("RegMatrix_VF.xlsx")
View(dat)
names(dat)#nombre de las variables
dim(dat)#dimensiones de la matriz dat (filas,columnas)

### Estructura de los datos

str(dat)#clasificaci√≥n de variables

#variables culitativas convertidas a un factor
#cambiamos nombres de variables por unos m√°s cortos por conveniencia en programaci√≥n
cobertura <- factor(dat$Cobertura)#cambio de dat$Cobertura a cobertura
paisaje <- factor(dat$Paisaje)
t.relieve <- factor(dat$Tipo_relieve)
f.terreno <- factor(dat$Forma_terreno)
mat.parental <-  factor(dat$Mat_parental)
orden <- factor(dat$Orden)

#tabla.estadisticas.cont(dat$OCSKGM)
# caracterizaci√≥n de la variable COS30CM 
par(mfrow=c(1,3))#mostrar en pantalla 3 gr√°ficos en una fila

hist(dat$COS30CM, prob=T,main="COS 30 cm", xlab="Stock COS (t.ha^-1)",col="lightblue", ylab="Densidad")#define histograma de la variable COS30CM

lines(density(dat$COS30CM))#l√≠nea de densidad de la variable COS30CM

rug(dat$COS30CM,col="blue")#agrega sobre el eje x (variable de inter√©s), los datos del eje x como peque√±as lineas verticales

grid(10,10)#rejilla que se establece sobre un gr√°fico, sirve para facilitar interpretaci√≥n en el dibujo 

curve(dnorm(x,mean=mean(dat$COS30CM),sd=sd(dat$COS30CM)), col="darkblue",lwd=2,add=T,yaxt="n")#curva de la distribuci√≥n normal, tomando como media de la campana la COS30CM

#PENDIENTE
text(400,0.004,bquote("("*bar(x)* "="*.(round(mean(dat$COS30CM),2))*")"))#agregar una anotacion dentro del histograma
###


plot(ecdf(dat$COS30CM),ylab="Frecuencia",xlab="Stock COS (t.ha^-1)",main="COS 30 cm")#grafica la funci√≥n de densidad acumulada de la variable COS30CM

boxplot(dat$COS30CM,col="lightblue",main="COS 30 cm",ylab="Stock COS (t.ha^-1)")#Boxplot de la variable COS30CM

points(mean(dat$COS30CM),col="red",pch=16)#puntos de la media de la variable COS30CM de color rojo y tama√±o 16

## Transformaci√≥n logar√≠tmica de la variable COS30CM 

par(mfrow=c(1,3))##mostrar en pantalla 3 gr√°ficos en una fila

hist(log(dat$COS30CM), prob=T,main="COS 30 cm", xlab="Stock COS (t.ha^-1)",col="lightblue",breaks=100, ylab="Densidad")#histograma de la variable COS30CM aplic√°ndole logaritmo

lines(density(log(dat$COS30CM)))#lineas de densidad de la variable COS30CM, aplic√°ndole log

rug(log(dat$COS30CM),col="blue")#agrega sobre el eje x (variable de inter√©s), los datos del eje x como peque√±as lineas verticales, aplic√°ndole log

grid(10,10)#rejilla que se establece sobre un gr√°fico, sirve para facilitar interpretaci√≥n en el dibujo 

curve(dnorm(x,mean=mean(log(dat$COS30CM)),sd=sd(log(dat$COS30CM))), col="darkblue",lwd=2,add=T,yaxt="n")#curva de dist. normal,donde la media y la desv. est√°ndar de la campana es el log. de la variable COS30CM 

#PENDIENTE#
text(2,0.5,bquote("("*bar(x)* "="*.(round(mean(log(dat$COS30CM)),2))*")"))
#PENDIENTE#

plot(ecdf(log(dat$COS30CM)),ylab="Frecuencia",xlab="Stock COS (t.ha^-1)",main="COS 30 cm")#grafica la funci√≥n de densidad acumulada del logaritno de la variable COS30CM 

boxplot(dat$COS30CM,col="lightblue",main="COS 30 cm",ylab="Stock COS (t.ha^-1)")#Boxplot de la variable COS30CM

points(mean(log(dat$COS30CM)),col="red",pch=16)##puntos de la media del logaritmo de la variable COS30CM de color rojo y tama√±o 16


## Recreamos el objeto con la ubicacion de los puntos
dat_sp<-dat
q<-dat_sp$W_1
class(dat_sp$W_1)
class(dat_sp$N_1)
dat_sp$N_1
N_1<-as.numeric(dat$N_1)
N_1
class(N_1)

dat_sp$W_1
N_1
class(N_1)
class(q)

dat_sp <- dat #renombrar la base dat a dat_sp
coordinates(dat_sp) <- ~ W_1 + N_1 ##coordenadas que est·n en dat_sp, de W_1 Y N_1 

## Ajustamos un modelo de regresion lineal multiple
## Pruebas de modelos ####

### Analisis de correlacion
#validar con Alejandro
names(dat_sp@data) #mirar si es una estructura S4 (@)

str(dat_sp)
names(dat_sp@data[,c(3:21)])
COR <- cor(as.matrix(dat_sp@data[,3]), as.matrix(dat_sp@data[,c(4:21)]))
COR #matriz de correlaci√≥n de todos los datos

x <- subset(melt(COR), value != 1 | value!= NA) #x matriz de correlaci√≥n, excluyendo los valores de 1 y los NA
x <- x[with(x, order(-abs(x$value))),]#define un orden para x
#as.character(x$X2[1:10]) # øquÈ es x2?
x[1:5,] #x de 5 filas con todas sus columnas

idx <- as.character(x$X2[c(1:5)])#vuelve caraCter, pero que es x2? 

dat2 <- dat[c('COS30CM', idx, 'N_1', 'W_1')]#dat2 conformado por 4 variables
names(dat2) #nombres de las variables que conforman dat2

#idx2 <-  as.character(c(idx,'Aquents','Folists','Udands','Udepts','Ustepts','SUP_AGUA',
#'A_HUM','VEG0','VEG_HERB','BOSQUES','CULT','A_HET','PASTOS','TERART'))


## Ahora, a diferencia del ejercicio 4, el modelo lo ajustamos solo con datos.model
## y no con todos los datos...!
dat2 <- dat2[dat2$COS30CM != 0,]# dat2 de la variable COS30CM diferentes de 0
modelo.MLR <- lm(COS30CM ~ . -N_1-W_1, data = dat2)# se hace un modelo y se le quitan las variebles N_1 y W_1

summary(modelo.MLR) #sumary de la regresi√≥n 
anova(modelo.MLR) #Analisis de varianza del modelo .MLR


## Hacemos seleccion de variables por stepwise
modelo.MLR.step <- step(modelo.MLR, direction="both") #al modelo se le aplica funciÛ stepwise, hacia atr·s 

summary(modelo.MLR.step) #resumen descriptivo del modelo, ya habiendo sacado variables no significativas  
anova(modelo.MLR.step) #se realiza an·lisis de varianza al modelo con stepwise

par(mfrow=c(2,2)) # plotea gr·fico de dos filas por dos columnas 
plot(modelo.MLR.step)#grafica el modelo con stepwise
par(mfrow=c(1,1)) #grafica un gr·fico

#Falta de multicolinealidad en las variables x: podemos comprobar esto mediante
#el calculo de los Factores de Inflacion de la Varianza (FIVs)
# lo ideal es: no haya dependecia lineal entre variables explicativas= multicolinealidad
library(car) #regresion aplicada
#Variables problematicas tienen sqrt(FIV) > 2
vif(modelo.MLR.step)#Calcula factores de variaciÛn-inflaciÛn y variaciÛn-inflaciÛn generalizada para modelos lineales, lineales generalizados, modelo stepwise en este caso
sqrt(vif(modelo.MLR.step)) #se calcula la raÌz cuadrada del vif ultimo 
modelo.MLR.step <- update(modelo.MLR.step, . ~ . -  DEM_90 )#actualizar· y (por defecto) volver· a ajustar un modelo, sin la variable DEM_90
sqrt(vif(modelo.MLR.step))#se calcula la raÌz cuadrada del vif ultimo
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - tx1mod3a )#actualizar· y (por defecto) volver· a ajustar un modelo, sin la variable tx1mod3a
sqrt(vif(modelo.MLR.step))#se calcula la raÌz cuadrada del vif ultimo
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - inssre3a )#actualizar· y (por defecto) volcer· a ajustar un modelo, sin la variable inssre3a 
sqrt(vif(modelo.MLR.step))#se calcula la raÌz cuadrada del vif ultimo
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - twisre3a )#actualizar· y (por defecto) volcer· a ajustar un modelo, sin la variable twisre3a
sqrt(vif(modelo.MLR.step))#se calcula la raÌz cuadrada del vif ultimo
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - TX4MOD3a )#actualizar· y (por defecto) volcer· a ajustar un modelo, sin la variable TX4MOD3a
sqrt(vif(modelo.MLR.step))#se calcula la raÌz cuadrada del vif ultimo
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - PEND_90)#actualizar· y (por defecto) volcer· a ajustar un modelo, sin la variable PEND_90 
sqrt(vif(modelo.MLR.step))#se calcula la raÌz cuadrada del vif ultimo
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - px4wcl3a)#actualizar· y (por defecto) volcer· a ajustar un modelo, sin la variable px4wcl3a 
sqrt(vif(modelo.MLR.step))#se calcula la raÌz cuadrada del vif ultimo
#Duda, todos los modelos se llaman igual y no se sabe cual es el mejor con que variable
#SUGERENCIA:Mirar si se puede implementar modelo akaike (aic) y criterio Schwarz (SIC)

#Usar la prueba de Bonferroni para valores atipicos: 
#La prueba de bonferroni 
#plantear rango intercuartÌlico u otro test para calculo de outliers al comienzo del an·lisis, seguido a analisis exploratorio
#en este punto no deberÌan ya haber outliers
outlierTest(modelo.MLR.step)#IdentificaciÛn de outliers(datos atipicos) del modelo.MLR.step
summary(modelo.MLR.step)#medidas resume n de modelo.MLR.step

'Covariables_VF_Rasterizadas/COV_VF.tif'#Documento tipo .tif
'Covariables_VF_Rasterizadas/namesCovariables_VF.rds'#Documento tipo .rds

# Lo convertimos a un SpatialGridDataFrame
getwd() #Se mira cual es el directorio de trabajo
dir() #enumera todos los archivos de un directorio

library(raster)#librerÌa cargada anteriormente

install.packages("raster") #instale el paquete r·ster
COV <- stack("COV_SIBU.tif")#Los vectores de apilamiento concatenan m˙ltiples vectores en un solo vector junto con un factor que indica dÛnde se originÛ cada observaciÛn
namesCov<- readRDS('NAMES_COV_SIB.rds')# se utiliza la funcion readRDS, funciones para escribir un solo objeto R en un archivo y restaurarlo.
names(COV)#nombre de variables de un solo vector o de varios(UN VECTOR CONCATENA CON VARIOS EN UN SOLO VECTOR),TOCA MIRAR QUE ARROJA  
names(COV) <- namesCov #a namesCov le asigna el nombre de names(COV)
names(COV) #compilar names(COV)

COV<- COV[[idx]] #covarianza de idx, vector de caracteres(tamaÒo 1x5)
#idx <- as.character(x$X2[c(1:5)])

# Project point data ##proyecciÛn de los puntos de las base de datos 
#dat2 <- dat[c('COS30CM', idx, 'N_1', 'W_1')]
dat2_sp <- dat2 # al vector dat2 se le da el nombre dat2_sp
coordinates(dat2_sp) <- ~ W_1 + N_1 #coordenadas que est·n en dat2_sp, de W_1 Y N_1

#############################################
#ACLARAR ESTA ESTRUCTURA##
dat2_sp@proj4string <- COV@crs
proy <- CRS("+proj=longlat +datum=WGS84")
dat2_sp@proj4string <- proy
###########################################

dat2_sp <- spTransform(dat2_sp, CRS("+init=epsg:32618")) #spTransform para proyecciÛn de mapas y transformaciÛn de sistma de modelos de ref. del elipsoide
#CRS:sistea de coordenadas del objeto
#epsg= sistema de coordenadas el software GIS

#Proyecta covarianzas para WGS84 UTM zone 18N Colombia
# project covariates to WGS84 UTM zone 18N Colombia
start <- Sys.time()#devuelve la idea del sistema de la fecha actual con y sin hora

COV <- projectRaster(COV, crs = CRS("+init=epsg:32618"), method='ngb')
#COV<- COV[[idx]]
#paquete raster:admite la pryecciÛn del vecino m·s cercano (interpolacion)
#projectRaster=para poner un raster en un nuevo CRS.
#Para utilizar la funcion projectRaster, se deben definir dos cosas: 1. el objeto que se quiere reproyectar 
#2. el sistema de coordenadas al cual quiero reproyectar(crs).
#metodo"ngb":vecinos cercanos
COV.sp <- as(COV, "SpatialGridDataFrame")#asignaciÛn de COV a un dataframe espacial

## Datos duplicados?
zerodist(dat_sp)#encuentra pares de puntos con coordenadas espaciales iguales

### RK model ##modelo kriging##

library(automap)#Este paquete realiza una interpolaciÛn autom·tica estimando autom·ticamente el variograma y luego llamando a gstat
#dat2_sp <- dat2_sp[dat2_sp$COS30CM != 0,]


OCS.krige <- autoKrige(formula = as.formula(modelo.MLR.step$call$formula), 
                       input_data = dat2_sp, 
                       new_data = COV.sp,
                       verbose = TRUE,
                       block = c(1000, 1000))
#autokrige: realiza kriging autom·tico al conjunto de datos
#formula: define la variable dependiente como un modelo lineal de variables independientes
#input_data: objeto de la clase spatialpointsdataframe que contiene los datos que se van a interpolar (dataframe de puntos espaciales)
#new_data:objeto con ubicaciones de prediccion, puede ser un conjunto de puntos, una cuadricula o poligono
#verbose:onjeto logico. si es TRUE= autokriging da inf. adicional sobre proceso de ajuste
#block:ransite especificacion para el tamao de bloque

##RETOMAR PARA DEFINIR QUE ES KRIGE_OUTPUT)
RKprediction <- raster(OCS.krige$krige_output[1])
RKpredsd <- raster(OCS.krige$krige_output[3])
################################################

summary(RKprediction)#resumen de resultados
summary(RKpredsd)##resumen de resultados
print(Sys.time() - start)#esto deberÌa dar 0
#start <- Sys.time()


memory.size()> memory.size(TRUE)#asignaciÛn de memoria acutual
memory.limit(40000)#informa o aumenta el limite vigente sobre asignacion actual


# Remove bad values
#de los valores de RKprediction que sea <0, >1000 son NA¥S 
values(RKprediction )[values(RKprediction ) < 0]  <- NA
values(RKprediction )[values(RKprediction ) > 1000]  <- NA
#de los valores de RKpredsd que sea >100 son NA¥S 
values(RKpredsd)[values(RKpredsd ) > 100]  <- NA


summary(RKprediction)##RESULTADOS DE EXTRACCI”N DE VALORES DE LA VARIABLE RKprediction
summary(RKpredsd)###RESULTADOS DE EXTRACCI”N DE VALORES DE LA VARIABLE RKpredsd

getwd()# Se mira cual es el directorio de trabajo 
 
#grafica las variables
plot(RKprediction)
plot(RKpredsd)

## Save results
#guarda resultados
getwd()
writeRaster(RKprediction, filename = "RK_PRED_11072018.tif",overwrite=T)
writeRaster(RKpredsd, filename = "RK_PREDSD_11072018.tif",overwrite=T)


#libreria necesaria
library(raster)
#importa raster
r1 <- raster ('RESULTADOS_PROCESOS/KRIGING/COL_OCS_RK_VF_DIC.tif')
#haces la operacion
r2 <- r1 *10
#guarda el raster generado
writeRaster(r2, 'RESULTADOS_PROCESOS/KRIGING/COL_OCS_RK_VF_DIC_TON_HA.tif')


#importas tu raster
r3 <- raster ('RESULTADOS_PROCESOS/KRIGING/COL_OCS_RKpredsd_VF_DIC.tif')
#haces la operacion
r4 <- r3 *10
#y guardas el raster generado
writeRaster(r4, 'RESULTADOS_PROCESOS/KRIGING/COL_OCS_RKpredsd_VF_DIC_TON_HA.tif')

plot(r2)
plot(r4)


####PREGUNTAR EL @####
#### Uncertainty estimation
# using LOO cross-validation

dat_sp = dat_sp[which(!duplicated(dat_sp@coords)), ]
###########################################################
OCS.krige.cv <- autoKrige.cv(formula = as.formula(modelo.MLR.step$call$formula), 
                             input_data = dat2_sp, nfold = 5)

summary(OCS.krige.cv)

str(RKprediction)