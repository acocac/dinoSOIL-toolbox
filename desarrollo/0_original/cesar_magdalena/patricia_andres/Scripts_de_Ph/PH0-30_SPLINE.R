##Analisis exploratorio, variable PH.0_30_spline###
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

# Cargan de datos
data <- read.csv('MatrixDatos.csv', sep=',')#lee los datos
View(data)#Visualizar la base data
names(data)#Nombre de las variables
nrow(data)#Numero de filas


##Omitir variables que no son de interes
datap <- data[,-c(1:11,13:15,21)] %>% na.omit
names(datap)#nombre de la nueva datap
ncol(datap)#numero de columnas de la base
View(datap)#visualizar la datap
str(datap)#Los seis primeros perfiles 

#arreglo de variables, nuevos nombres y se vuelven as.numeric
class(datap$pH.0_30_spline)
ph1_spline1<-as.numeric(datap$pH.0_30_spline)
class(datap$dem)
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
datap1<-data.frame(ph1_spline1, DEM, AnHill, Slope, Aspect, CNBL, Conv_Ind,
                   CSC, FA, LC, LSfactor, MRRTF, MRVBF, Neg_Op, Pos_Op, RSP, skyVF,
                   TRI, TPI, TWI, VD, VDCN, visSky, ndvi, tipo_relieve, Clima)
class(datap1$Clima)#Clase de variable Clima
datap1$Clima<-as.factor(datap1$Clima)#se vuelve un factor la variable Clima
View(datap1$Clima)#Visualizar la variable Clima
levels(datap1$Clima)#Niveles de Clima
length(datap1)#Tamaño de Clima
nrow(datap1)#Numero de filas de datap1
ncol(datap1)#Numero de columnas de datap1
names(datap1)#nombres de las variables de datap1
View(datap1)#visualizar la base datap1

####
####Pairs.pannel##
#variables#
##Reconocimiento de las 6 primeras variables tipo numeric
datpairs<-data.frame(Slope,LSfactor,CNBL,visSky,RSP,DEM)
length(datpairs)
str(datpairs)
pairs.panels(datpairs)
chart.Correlation(datpairs,hist=T)
######################mirar esto

##Correlaciones de la variable PH (0-30, SPLINE) con las demás covariables
matrixcor<-data.frame(ph1_spline1, DEM, AnHill, Slope, Aspect, CNBL, Conv_Ind,
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

levels(datap1$Clima)
View(datap1$Clima)
####Diagramas de dispersion####
##DIAGRAMAS DE DISPERSION CON RESPECTO AL CLIMA

#Variable PH con DEM
ggplot(data=datap1, aes(x=DEM, y=ph1_spline1, colour=clima)) +
  geom_point() +
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs. DEM") +
  xlab("Modelo Digital de Elevación")+
  ylab("Ph, nivel:0-30,Spline")

#Variable PH con AnHill
ggplot(datap1, aes(y=ph1_spline1, x=AnHill, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs. AnHill") +
  xlab("Analytical Hillshade")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()
View(datap1$AnHill)

#Variable PH con Slope
ggplot(datap1, aes(y=ph1_spline1, x=Slope, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs. Slope") +
  xlab("Slope")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con Aspect
ggplot(datap1, aes(y=ph1_spline1, x=Aspect, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs. Aspect") +
  xlab("Aspecto")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con CNBL
ggplot(datap1, aes(y=ph1_spline1, x=CNBL, colour=clima)) +
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.channel network base level") +
  xlab("channel network base level")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con CHBL
ggplot(datap1, aes(y=ph1_spline1, x=CNBL, colour=clima)) +
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.CNBL") +
  xlab("ChannelNetworkBaseLevel")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con Conv_Ind
ggplot(datap1, aes(y=ph1_spline1, x=Conv_Ind, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Conv_Ind") +
  xlab("Convergence Index")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con CSC
ggplot(datap1, aes(y=ph1_spline1, x=CSC, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.CSC") +
  xlab("Cross Sectional Curvature")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con FLOWACCUMULATION
ggplot(datap1, aes(y=ph1_spline1, x=FA, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Flow Accum.") +
  xlab("Flow Accumulation")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con LONGITUDINALCURVATURE
ggplot(datap1, aes(y=ph1_spline1, x=LC, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Long.Curv.") +
  xlab("Longitudinal Curvature")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con LSFactor
ggplot(datap1, aes(y=ph1_spline1, x=LSfactor, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Factor LS") +
  xlab("Factor LS")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con MRRTF
ggplot(datap1, aes(y=ph1_spline1, x=MRRTF, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.MRRTF") +
  xlab("Índice multiresolución de la planitud superior de la cresta")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con MRVBF
ggplot(datap1, aes(y=ph1_spline1, x=MRVBF, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.MRVBF") +
  xlab("Multiresolution Index of Valley Bottom Flatness")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con neg_op
ggplot(datap1, aes(y=ph1_spline1, x=Neg_Op, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Negative Openness") +
  xlab("Negative Openness")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con Pos_op
ggplot(datap1, aes(y=ph1_spline1, x=Pos_Op, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Positive Openness") +
  xlab("Positive Openness")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con RSP
ggplot(datap1, aes(y=ph1_spline1, x=RSP, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.RSP") +
  xlab("Relative Slope Position")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con SkyVF
ggplot(datap1, aes(y=ph1_spline1, x=skyVF, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.skyVF") +
  xlab("Sky View Factor")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con TRI
ggplot(datap1, aes(y=ph1_spline1, x=TRI, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.TRI") +
  xlab("Terrain Ruggedness Index")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con TPI
ggplot(datap1, aes(y=ph1_spline1, x=TPI, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.TPI") +
  xlab("Topographic Position Index ")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con TWI
ggplot(datap1, aes(y=ph1_spline1, x=TWI, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Topographic wetness index") +
  xlab("Topographic wetness index")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con VD
ggplot(datap1, aes(y=ph1_spline1, x=VD, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Velley Depth") +
  xlab("Velley Depth")+
  ylab("Ph, nivel:0-30,Spline")+
  labs(fill="Clima")+
  geom_point()

#variable PH con VDCN
ggplot(datap1, aes(y=ph1_spline1, x=VDCN, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.VDCN") +
  xlab("Vertical Distance Channel Network")+
  ylab("Ph, nivel:0-30,Spline")+
  labs(fill="Clima")+
  geom_point()

#variable PH con visSky
ggplot(datap1, aes(y=ph1_spline1, x=visSky, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.Visible Sky") +
  xlab("Visible Sky")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con ndvi
ggplot(datap1, aes(y=ph1_spline1, x=ndvi, colour=clima)) + 
  labs(fill="Clima")+
  ggtitle("PH(0-30, spline)Vs.ndvi") +
  xlab("Ind. de Vegetación Diferencia Normalizada ")+
  ylab("Ph, nivel:0-30,Spline")+
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
ggplot(datap1, aes(y=ph1_spline1, x=DEM, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.DEM") +
  xlab("Modelo digital de elevación")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#Variable PH con AnHill
ggplot(datap1, aes(y=ph1_spline1, x=AnHill, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs. AnHill") +
  xlab("Analytical Hillshade")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#Variable PH con Slo
ggplot(datap1, aes(y=ph1_spline1, x=Slope, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs. Slo") +
  xlab("Slope")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con Aspect
ggplot(datap1, aes(y=ph1_spline1, x=Aspect, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs. Aspect") +
  xlab("Aspecto")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con CNBL
ggplot(datap1, aes(y=ph1_spline1, x=CNBL, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Channel Network Base Level") +
  xlab("Channel Network Base Level")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con Conv_Ind
ggplot(datap1, aes(y=ph1_spline1, x=Conv_Ind, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Convergence Index") +
  xlab("Convergence Index")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con Cross Sectional Curvature
ggplot(datap1, aes(y=ph1_spline1, x=CSC, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.CSC") +
  xlab("Cross Sectional Curvature")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con FA
ggplot(datap1, aes(y=ph1_spline1, x=FA, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.") +
  xlab("Flow Accum")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con Longitudinal Curvature
ggplot(datap1, aes(y=ph1_spline1, x=LC, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.LC") +
  xlab("Longitudinal Curvature")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con LSFactor
ggplot(datap1, aes(y=ph1_spline1, x=LSfactor, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Factor LS") +
  xlab("Factor LS")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con MRRTF
ggplot(datap1, aes(y=ph1_spline1, x=MRRTF, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.MRRTF") +
  xlab("Índice multiresolución de la planitud superior de la cresta")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con MRVBF
ggplot(datap1, aes(y=ph1_spline1, x=MRVBF, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.MRVBF") +
  xlab("Multiresolution Index of Valley Bottom Flatness")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con neg_op
ggplot(datap1, aes(y=ph1_spline1, x=Neg_Op, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Ubic.Paisaje") +
  xlab("Ubic.Paisaje Negativo")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con Pos_op
ggplot(datap1, aes(y=ph1_spline1, x=Pos_Op, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Ubicación paisaje") +
  xlab("Ubic. Paisaje positivo")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con RSP
ggplot(datap1, aes(y=ph1_spline1, x=RSP, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.RSP") +
  xlab("Relative Slope Position")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con SkyVF
ggplot(datap1, aes(y=ph1_spline1, x=skyVF, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.skyVF") +
  xlab("Sky View Factor")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con TRI
ggplot(datap1, aes(y=ph1_spline1, x=TRI, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.TRI") +
  xlab("Terrain Ruggedness Index")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con TPI
ggplot(datap1, aes(y=ph1_spline1, x=TPI, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.TPI") +
  xlab("Multi-Scale Topographic Position Index ")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con TWI
ggplot(datap1, aes(y=ph1_spline1, x=TWI, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Topographic Wetness Index") +
  xlab("Topographic Wetness Index")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con VD
ggplot(datap1, aes(y=ph1_spline1, x=VD, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Velley Depth") +
  xlab("Velley Depth")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#variable PH con VDCN
ggplot(datap1, aes(y=ph1_spline1, x=VDCN, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Vertical Distance Channel Network") +
  xlab("Vertical Distance Channel Network")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con visSky
ggplot(datap1, aes(y=ph1_spline1, x=visSky, colour=Relieve)) +
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.Visible Sky") +
  xlab("Visible Sky")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()

#variable PH con ndvi
ggplot(datap1, aes(y=ph1_spline1, x=ndvi, colour=Relieve)) + 
  labs(fill="Relieve")+
  ggtitle("PH(0-30, spline)Vs.ndvi") +
  xlab("Indice de Vegetación de la Diferencia Normalizada ")+
  ylab("Ph, nivel:0-30,Spline")+
  geom_point()


#################################
##Graficos de Boxplot###########
#Boxplot de cada variable con respecto al Clima#
names(datap1)#nombres de variables que hacen parte de la base datap1

#Boxplot de DEM
ggplot(data = datap1,aes(x=clima,y =DEM)) +
  ylab("Modelo digital de elevación")+
  ggtitle('Boxplot DEM') + 
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Boxplot de AnHill
ggplot(data = datap1, aes(x =clima, y =AnHill)) +
  ylab("Analytical Hillshade")+
  ggtitle('Boxplot AnHill')+ 
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Boxplot de Slope
ggplot(data = datap1, aes(x =clima, y =Slope)) +
  ylab("Slope")+
  ggtitle('Boxplot Slope')+
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Boxplot de Aspect
ggplot(data = datap1, aes(x =clima, y =Aspect)) +
  ylab("Aspect")+
  ggtitle('Boxplot Aspect')+
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

#Boxplot de CNBL
ggplot(data = datap1, aes(x =clima, y =CNBL)) +
  ylab("Channel network base level")+
  ggtitle('Boxplot Channel network base level')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#Boxplot de Ph0-30,Spline
ggplot(data = datap1, aes(x =clima, y =ph1_spline1)) +
  ylab("Ph,nivel:0-30,Spline")+
  ggtitle('Boxplot Ph,nivel:0-30,Spline')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#Boxplot de conv_Ind
ggplot(data = datap1,aes(x =clima, y =Conv_Ind)) +
  ylab("Convergence Index")+
  ggtitle('Boxplot Convergence Index')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#Boxplot de CSC
ggplot(data = datap1,aes(x =clima, y =CSC)) +
  ylab("Cross Sectional Curvature")+
  ggtitle('Boxplot Cross Sectional Curvature')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Boxplot de Flow Accum
ggplot(data = datap1,aes(x =clima, y =FA)) +
  ylab("Flow Accum")+
  ggtitle('Boxplot Flow Accum')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Boxplot de Longitudinal Curvature
ggplot(data = datap1,aes(x =clima, y =LC)) +
  ylab("Longitudinal Curvature")+
  ggtitle('Boxplot Longitudinal Curvature')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Boxplot de LsFactor
ggplot(data = datap1, aes(x =clima, y =LSfactor)) +
  ylab("Factor LS")+
  ggtitle('Boxplot Factor LS')+
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##Boxplot de MRRTF
ggplot(data = datap1, aes(x=clima, y =MRRTF)) +
  ylab("Índice multiresolución de la planitud superior de la cresta")+
  ggtitle('Boxplot Índice multiresolución de la planitud superior de la cresta')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##Boxplot de MRVBF
ggplot(data = datap1, aes(x =clima, y =MRVBF)) +
  ylab("Multiresolution Index of Valley Bottom Flatness")+
  ggtitle('Boxplot Multiresolution Index of Valley Bottom Flatness')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##Box plot de Neg_Op
ggplot(data = datap1, aes(x =clima, y =Neg_Op)) +
  ylab("Ubic.Paisaje Negativo")+
  ggtitle('Boxplot Ubic.Paisaje Negativo')+
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##Boxplot de Pos_Op
ggplot(data = datap1, aes(x =clima, y =Pos_Op)) +
  ylab("Ubic. Paisaje positivo")+
  ggtitle('Boxplot Ubic. Paisaje positivo')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##Boxplot de RSP
ggplot(data = datap1, aes(x =clima, y =RSP)) +
  ylab("Relative Slope Position")+
  ggtitle('Boxplot Relative Slope Position')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##Boxplot de skyVF
ggplot(data = datap1, aes(x =clima, y =skyVF)) +
  ylab("Sky View Factor")+
  ggtitle('Boxplot Sky View Factor')+
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##Boxplot de TRI
ggplot(data = datap1, aes(x =clima, y =TRI)) +
  ylab("Terrain Ruggedness Index")+
  ggtitle('Boxplot Terrain Ruggedness Index')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##Boxplot de TPI
ggplot(data = datap1, aes(x =clima, y =TPI)) +
  ylab("Multi-Scale Topographic Position Index")+
  ggtitle('Boxplot Multi-Scale Topographic Position Index')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

#Boxplot de TWI
ggplot(data = datap1, aes(x =clima, y =TWI)) +
  ylab("Topographic wetness index")+
  ggtitle('Boxplot Topographic wetness index')+
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

##Boxplot de VD
ggplot(data = datap1, aes(x =clima, y =VD)) +
  ylab("Velley Depth")+
  ggtitle('Boxplot Velley Depth')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

datap1$VDCN

##Boxplot de VDCN
ggplot(data = datap1, aes(x =clima, y =VDCN)) +
  ylab("Vertical Distance Channel Network")+
  ggtitle('Boxplot Vertical Distance Channel Network')+
  geom_boxplot(aes(fill=clima))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##Boxplot de visSky1
ggplot(data = datap1, aes(x =clima, y =visSky)) +
  ylab("Visible Sky")+
  ggtitle('Boxplot Visible Sky')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


##Boxplot de NDVI
ggplot(data = datap1, aes(x =clima, y =ndvi)) +
  ylab("Indice de Vegetación de la Diferencia Normalizada")+
  ggtitle('Boxplot Indice de Vegetación de la Diferencia Normalizada')+
  geom_boxplot(aes(fill=clima)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


###############################################################################
