##Analisis exploratorio, variable PH.30_100_sumasponderadas###
#Carga de paquetes#

library(readxl)
library(tidyr)
library(ggplot2)
library(psych)#calculo de correlaciones
library(psych)#Pairs.panels
library(PerformanceAnalytics)#chart.Correlation

##Limpiar espacio de trabajo##
rm(list=ls())

##Carga de datos(matriz de regresion)##
setwd("C:/Users/usuario/Desktop/Patricia/IGAC/Proyectos IGAC/PROYECTO II/MAPEO DIGITAL/Maching Learning/Ultima matriz de regresion PH")

# Se Cargan los datos de los splines
data <- read.csv('MatrixDatos.csv', sep=',')#lee los datos
View(data)#ver la base data
names(data)#nombre de las variables
nrow(data)#numero de filas de la base data

#arreglo de variables, nuevos nombres y se vuelven as.numeric
datap <- data[,-c(1:14,21)] %>% na.omit
names(datap)#nombres de las variables de la base datap
length(datap)#tamaño de la base datap
View(datap)#ver la base datap
str(datap)#los seis primero perfiles

#arreglo de variables#arreglo de variables, nuevos nombres y se vuelven as.numericph2_sum.pond2<-as.numeric(datap$pH.30_100_Sum.Pond)
ph2_sum.pond2<-as.numeric(datap$pH.30_100_Sum.Pond)
DEM<-as.numeric(datap$dem)
AnHill<-as.numeric(datap$AnalyticalHillshading)
Slope<-as.numeric(datap$Slope)
Aspect<-as.numeric(datap$Aspect)
CNBL<-as.numeric(datap$ChanneNetworkBaseLevel)#
Conv_Ind<-as.numeric(datap$ConvergenceIndex)
CSC<-as.numeric(datap$CrossSectionalCurvature)#
FA<-as.numeric(datap$FlowAccumulation)
LC<-as.numeric(datap$LongitudinalCurvature)
LSfactor<-as.numeric(datap$LSFactor)
MRRTF<-as.numeric(datap$MRRTF)
MRVBF<-as.numeric(datap$MRVBF)
Neg_Op<-as.numeric(datap$NegativeOpenness)
Pos_Op<-as.numeric(datap$PositiveOpenness)
RSP<-as.numeric(datap$RelativeSlopePosition)
skyVF<-as.numeric(datap$SkyViewFactor)
TRI<-as.numeric(datap$TerrainRuggednessIndex)
TPI<-as.numeric(datap$TopographicPositionIndex)
TWI<-as.numeric(datap$TopographicWetnessIndex)
VD<-as.numeric(datap$ValleyDepth)
VDCN<-as.numeric(datap$VerticalDistanceChannelNetwork)
visSky<-as.numeric(datap$VisibleSky)
ndvi<-as.numeric(datap$ndvi)
tipo_relieve<-datap$tipo_relieve
Clima<-datap$clima

#Nueva base con las nuevas variables
datap1<-data.frame(ph2_sum.pond2, DEM, AnHill, Slope, Aspect, CNBL, Conv_Ind,
                   CSC, FA, LC, LSfactor, MRRTF, MRVBF, Neg_Op, Pos_Op, RSP, skyVF,
                   TRI, TPI, TWI, VD, VDCN, visSky, ndvi, tipo_relieve, Clima)
class(datap1$Clima)#tipo de variable clima
datap1$Clima<-as.factor(datap1$Clima)#clami como un factor
names(datap1)#nombres de las variables de la base datap1
View(datap1$Clima)#ver la variable clima
levels(datap1$Clima)#niveles de a variable clima
length(datap1)#tamaño de la base datap1
nrow(datap1)#numero de filas de la base datap1
ncol(datap1)#numero de columnas de la base datap1
View(datap1)#ver la base datap1

##Correlaciones de la variable PH (0-30, SPLINE) con las demÃƒÂ¡s covariables
matrixcor<-data.frame(ph2_sum.pond2, DEM, AnHill, Slope, Aspect, CNBL, Conv_Ind,
                      CSC, FA, LC, LSfactor, MRRTF, MRVBF, Neg_Op, Pos_Op, RSP, skyVF,
                      TRI, TPI, TWI, VD, VDCN, visSky, ndvi)
cor(matrixcor)
#########################################################################

##Etiquetas de los niveles de la variable Clima##
datap1$clima<-gsub(14,"(Templado, húmedo)",datap1$Clima)
datap1$clima<-gsub(13,"(Subnival, Pluvial)",datap1$clima)
datap1$clima<-gsub(1, "(Cálido, húmedo)",datap1$clima)
datap1$clima<-gsub(2, "(Cálido, muy seco)",datap1$clima)
datap1$clima<-gsub(3, "(Cálido, seco)",datap1$clima)
datap1$clima<-gsub(7, "(Frío, húmedo)",datap1$clima)
levels(datap1$Clima)#niveles de la variable clima
View(datap1$Clima)#ver la variable clima

####Diagramas de dispersion####
##DIAGRAMAS DE DISPERSION CON RESPECTO AL CLIMA
#Variable PH con DEM

ggplot(data=datap1, aes(x=DEM, y=ph2_sum.pond2, colour=clima)) +
  geom_point() +
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs. DEM") +
  xlab("Modelo Digital de Elevación")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")

#Variable PH con AnHill
ggplot(datap1, aes(y=ph2_sum.pond2, x=AnHill, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs. AnHill") +
  xlab("Analytical Hillshade")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#Variable PH con Slope
ggplot(datap1, aes(y=ph2_sum.pond2, x=Slope, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs. Slope") +
  xlab("Slope")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con Aspect
ggplot(datap1, aes(y=ph2_sum.pond2, x=Aspect, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs. Aspect") +
  xlab("Aspecto")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con CNBL
ggplot(datap1, aes(y=ph2_sum.pond2, x=CNBL, colour=clima)) +
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.channel network base level") +
  xlab("channel network base level")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con CHBL
ggplot(datap1, aes(y=ph2_sum.pond2, x=CNBL, colour=clima)) +
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.CNBL") +
  xlab("ChannelNetworkBaseLevel")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con Conv_Ind
ggplot(datap1, aes(y=ph2_sum.pond2, x=Conv_Ind, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Conv_Ind") +
  xlab("Convergence Index")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con CSC
ggplot(datap1, aes(y=ph2_sum.pond2, x=CSC, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.CSC") +
  xlab("Cross Sectional Curvature")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con FLOWACCUMULATION
ggplot(datap1, aes(y=ph2_sum.pond2, x=FA, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Flow Accum.") +
  xlab("Flow Accumulation")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con LONGITUDINALCURVATURE
ggplot(datap1, aes(y=ph2_sum.pond2, x=LC, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Long.Curv.") +
  xlab("Longitudinal Curvature")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con LSFactor
ggplot(datap1, aes(y=ph2_sum.pond2, x=LSfactor, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Factor LS") +
  xlab("Factor LS")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con MRRTF
ggplot(datap1, aes(y=ph2_sum.pond2, x=MRRTF, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.MRRTF") +
  xlab("Indice multiresolución de la planitud superior de la cresta")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con MRVBF
ggplot(datap1, aes(y=ph2_sum.pond2, x=MRVBF, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.MRVBF") +
  xlab("Multiresolution Index of Valley Bottom Flatness")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()


#variable PH con neg_op
ggplot(datap1, aes(y=ph2_sum.pond2, x=Neg_Op, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Negative Openness") +
  xlab("Negative Openness")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con Pos_op
ggplot(datap1, aes(y=ph2_sum.pond2, x=Pos_Op, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Positive Openness") +
  xlab("Positive Openness")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con RSP
ggplot(datap1, aes(y=ph2_sum.pond2, x=RSP, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.RSP") +
  xlab("Relative Slope Position")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con SkyVF
ggplot(datap1, aes(y=ph2_sum.pond2, x=skyVF, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.skyVF") +
  xlab("Sky View Factor")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con TRI
ggplot(datap1, aes(y=ph2_sum.pond2, x=TRI, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.TRI") +
  xlab("Terrain Ruggedness Index")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con TPI
ggplot(datap1, aes(y=ph2_sum.pond2, x=TPI, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.TPI") +
  xlab("Topographic Position Index ")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con TWI
ggplot(datap1, aes(y=ph2_sum.pond2, x=TWI, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Topographic wetness index") +
  xlab("Topographic wetness index")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#variable PH con VD
ggplot(datap1, aes(y=ph2_sum.pond2, x=VD, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Velley Depth") +
  xlab("Velley Depth")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  labs(fill="Clima")+
  geom_point()

#variable PH con VDCN
ggplot(datap1, aes(y=ph2_sum.pond2, x=VDCN, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.VDCN") +
  xlab("Vertical Distance Channel Network")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  labs(fill="Clima")+
  geom_point()

#variable PH con visSky
ggplot(datap1, aes(y=ph2_sum.pond2, x=visSky, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Visible Sky") +
  xlab("Visible Sky")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()


#variable PH con ndvi
ggplot(datap1, aes(y=ph2_sum.pond2, x=ndvi, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.ndvi") +
  xlab("Ind. de Vegetación Diferencia Normalizada ")+
  ylab("Ph, nivel:30-100, Sum.Ponderada")+
  geom_point()

#############################################################
##DIAGRAMAS DE DISPERSION CON RESPECTO AL tipo de relieve
#SUGERENCIA: La interpretación de las variables respecto al
#tipo de relieve no resulta tan facil de interpretar, ya que
#la variable tipo relieve tiene 14 niveles(paleta de colores)
View(tipo_relieve)#Ver variable Tipo de Relieve
datap1$tipo_relieve<-as.factor(datap1$tipo_relieve)#Volver Factor el Tipo de Relieve
levels(datap1$tipo_relieve)#Niveles de la variable Tipo de Relieve

datap1$Relieve<-gsub(14,"(Fila y viga)",datap1$tipo_relieve)
datap1$Relieve<-gsub(15,"(Abanico aluvial antiguo)",datap1$Relieve)
datap1$Relieve<-gsub(16,"(Abanico aluvial reciente)",datap1$Relieve)
datap1$Relieve<-gsub(17,"(Abanico aluvial subreciente)",datap1$Relieve)
datap1$Relieve<-gsub(20,"(Cumbre)",datap1$Relieve)
datap1$Relieve<-gsub(21,"(Cono de deyección)",datap1$Relieve)
datap1$Relieve<-gsub(1, "(Terraza aluvial nivel 1)",datap1$Relieve)
datap1$Relieve<-gsub(2, "(Colina)",datap1$Relieve)
datap1$Relieve<-gsub(3, "(Glacis de acumulación)",datap1$Relieve)
datap1$Relieve<-gsub(4, "(Loma)",datap1$Relieve)
datap1$Relieve<-gsub(5, "(Valle estrecho)",datap1$Relieve)
datap1$Relieve<-gsub(6, "(Depresión)",datap1$Relieve)
datap1$Relieve<-gsub(7, "(Vallecito)",datap1$Relieve)
datap1$Relieve<-gsub(8, "(Plano de inundación de río meándrico activo)",datap1$Relieve)
datap1$Relieve<-gsub(9, "(Plano de inundación de río meándrico inactivo)",datap1$Relieve)


#Varriable relieve con DEM
ggplot(datap1, aes(y=ph2_sum.pond2, x=DEM, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.DEM") +
  xlab("Modelo digital de elevación")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#Variable PH con AnHill
ggplot(datap1, aes(y=ph2_sum.pond2, x=AnHill, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs. AnHill") +
  xlab("Analytical Hillshade")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#Variable PH con Slo
ggplot(datap1, aes(y=ph2_sum.pond2, x=Slope, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs. Slo") +
  xlab("Slope")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con Aspect
ggplot(datap1, aes(y=ph2_sum.pond2, x=Aspect, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs. Aspect") +
  xlab("Aspecto")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con CNBL
ggplot(datap1, aes(y=ph2_sum.pond2, x=CNBL, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Channel Network Base Level") +
  xlab("Channel Network Base Level")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con Conv_Ind
ggplot(datap1, aes(y=ph2_sum.pond2, x=Conv_Ind, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Convergence Index") +
  xlab("Convergence Index")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con Cross Sectional Curvature
ggplot(datap1, aes(y=ph2_sum.pond2, x=CSC, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.CSC") +
  xlab("Cross Sectional Curvature")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con FA
ggplot(datap1, aes(y=ph2_sum.pond2, x=FA, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.") +
  xlab("Flow Accum")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con Longitudinal Curvature
ggplot(datap1, aes(y=ph2_sum.pond2, x=LC, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.LC") +
  xlab("Longitudinal Curvature")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con LSFactor
ggplot(datap1, aes(y=ph2_sum.pond2, x=LSfactor, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Factor LS") +
  xlab("Factor LS")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con MRRTF
ggplot(datap1, aes(y=ph2_sum.pond2, x=MRRTF, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.MRRTF") +
  xlab("Índice multiresolución de la planitud superior de la cresta")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con MRVBF
ggplot(datap1, aes(y=ph2_sum.pond2, x=MRVBF, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.MRVBF") +
  xlab("Multiresolution Index of Valley Bottom Flatness")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con neg_op
ggplot(datap1, aes(y=ph2_sum.pond2, x=Neg_Op, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Ubic.Paisaje") +
  xlab("Ubic.Paisaje Negativo")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con Pos_op
ggplot(datap1, aes(y=ph2_sum.pond2, x=Pos_Op, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Ubicación paisaje") +
  xlab("Ubic. Paisaje positivo")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con RSP
ggplot(datap1, aes(y=ph2_sum.pond2, x=RSP, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.RSP") +
  xlab("Relative Slope Position")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con SkyVF
ggplot(datap1, aes(y=ph2_sum.pond2, x=skyVF, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.skyVF") +
  xlab("Sky View Factor")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con TRI
ggplot(datap1, aes(y=ph2_sum.pond2, x=TRI, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.TRI") +
  xlab("Terrain Ruggedness Index")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con TPI
ggplot(datap1, aes(y=ph2_sum.pond2, x=TPI, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.TPI") +
  xlab("Multi-Scale Topographic Position Index ")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con TWI
ggplot(datap1, aes(y=ph2_sum.pond2, x=TWI, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Topographic Wetness Index") +
  xlab("Topographic Wetness Index")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con VD
ggplot(datap1, aes(y=ph2_sum.pond2, x=VD, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Velley Depth") +
  xlab("Velley Depth")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()


#variable PH con VDCN
ggplot(datap1, aes(y=ph2_sum.pond2, x=VDCN, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Vertical Distance Channel Network") +
  xlab("Vertical Distance Channel Network")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con visSky
ggplot(datap1, aes(y=ph2_sum.pond2, x=visSky, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.Visible Sky") +
  xlab("Visible Sky")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#variable PH con ndvi
ggplot(datap1, aes(y=ph2_sum.pond2, x=ndvi, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(30-100, Sum.Ponderada)Vs.ndvi") +
  xlab("Indice de Vegetación de la Diferencia Normalizada ")+
  ylab("Ph, nivel:30-100,Sum.Ponderada")+
  geom_point()

#################################
##Graficos de Boxplot###########
#Boxplot de la varriable Ph0-30,spline con respecto al Clima#
#Boxplot de Ph0-30,Sum.Ponderada
ggplot(data = datap1, aes(x =clima, y =ph2_sum.pond2)) +
  ylab("Ph,nivel:30-100,Sum.Ponderada")+
  ggtitle('Boxplot Ph,nivel:30-100,Sum.Ponderada')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

###############################################################################


