##Analisis exploratorio, variable PH.0_30_spline###
#Carga de paquetes#


library(readxl)
library(tidyr)
library(ggplot2)

##Limpiar espacio de trabajo##
rm(list=ls())

##Carga de datos(matriz de regresión)##
setwd("C:/Users/usuario/Desktop/Patricia/IGAC/Proyectos IGAC/PROYECTO II/MAPEO DIGITAL/Maching Learning")

# Se Cargan los datos de los splines
data <- read.csv('MatrixRegresion_29092020.csv', sep=';')#lee los datos
View(data)
names(data)#nombre de las variables
nrow(data)


##Eliminación de variables que no son de interes
names(data)
datap <- data[,-c(1:12,14,16)] %>% na.omit
data$ORDEN <- factor(data$ORDEN)
names(datap)
length(datap)
str(datap)

#arreglo de variables
class(datap$pH.0_30_spline)
ph2_sum<-as.numeric(datap$pH.0_30_Sum.Pond)
ph1_spline1<-as.numeric(datap$pH.0_30_spline)
class(ph2_sum)
class(datap$DEM)
DEM1<-as.numeric(datap$DEM)
AnHill1<-as.numeric(datap$AnHill)
Slo1<-as.numeric(datap$Slo)
Aspect1<-as.numeric(datap$Aspect)
Plan_curv1<-as.numeric(datap$Plan_Curv)
prof_curv1<-as.numeric(datap$Prof_Curv)
Conv_Ind1<-as.numeric(datap$Conv_Ind)
Closs_Dep1<-as.numeric(datap$Closs_Dep)#son 0 todo
TotCatchAr1<-as.numeric(datap$TotCatchAr)
TWI1<-as.numeric(datap$TWI)
LSfactor1<-as.numeric(datap$LSfactor)
CHBL1<-as.numeric(datap$CHBL)
CND1<-as.numeric(datap$CND)
VD1<-as.numeric(datap$VD)
RSP1<-as.numeric(datap$RSP)
MRVBF1<-as.numeric(datap$MRVBF)
MRRTF1<-as.numeric(datap$MRRTF)
TRI1<-as.numeric(datap$TRI)
visSky1<-as.numeric(datap$visSky)
skyVF1<-as.numeric(datap$SkyVF)
Pos_Op1<-as.numeric(datap$Pos_Op)
Neg_Op1<-as.numeric(datap$Neg_Op)
TPI1<-as.numeric(datap$TPI)
clima1<-datap$clima
tipo_relieve1<-datap$tipo_relieve
ndvi1<-as.numeric(datap$ndvi)
datap1<-data.frame(ph1_spline1,ph2_sum, DEM1, AnHill1, Slo1, Aspect1, Plan_curv1, 
                   prof_curv1, Conv_Ind1, TotCatchAr1, TWI1, 
                   LSfactor1, CHBL1, CND1, VD1, RSP1, MRVBF1, MRRTF1, TRI1,
                   visSky1,skyVF1, Pos_Op1, Neg_Op1, TPI1, clima1, tipo_relieve1,
                   ndvi1)
length(datap1)
names(datap1)


##calculo de correlaciones
cor(datap1)
names(datap1)
pairs(datap1[,4:13])

####Diagramas de dispersion####
##DIAGRAMAS DE DISPERSION CON RESPECTO AL CLIMA
#Variable PH1 con DEM1
clima1<-as.factor(datap1$clima1)
class(clima1)
ggplot(datap1, aes(y=ph1_spline1, x=DEM1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#Variable PH con AnHill1
ggplot(datap1, aes(y=ph1_spline1, x=AnHill1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#Variable PH con Slo1
ggplot(datap1, aes(y=ph1_spline1, x=Slo1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Aspect
ggplot(datap1, aes(y=ph1_spline1, x=Aspect1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Plan_curv1
ggplot(datap1, aes(y=ph1_spline1, x=Plan_curv1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#Variable PH con prof_curv1
ggplot(datap1, aes(y=ph1_spline1, x=prof_curv1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Conv_Ind1
ggplot(datap1, aes(y=ph1_spline1, x=Conv_Ind1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Totcatchar1
ggplot(datap1, aes(y=ph1_spline1, x=TotCatchAr1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con TW1
ggplot(datap1, aes(y=ph1_spline1, x=TWI1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con LSFactor1
ggplot(datap1, aes(y=ph1_spline1, x=LSfactor1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con CHBL1
ggplot(datap1, aes(y=ph1_spline1, x=CHBL1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con CND1
ggplot(datap1, aes(y=ph1_spline1, x=CND1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con VD1
ggplot(datap1, aes(y=ph1_spline1, x=VD1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con RSP1
ggplot(datap1, aes(y=ph1_spline1, x=RSP1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con MRVBF1
ggplot(datap1, aes(y=ph1_spline1, x=MRVBF1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con MRRTF1
ggplot(datap1, aes(y=ph1_spline1, x=MRRTF1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con TRI1
ggplot(datap1, aes(y=ph1_spline1, x=TRI1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con visSky1
ggplot(datap1, aes(y=ph1_spline1, x=visSky1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con SkyVF1
ggplot(datap1, aes(y=ph1_spline1, x=skyVF1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Pos_op1
ggplot(datap1, aes(y=ph1_spline1, x=Pos_Op1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con neg_op1
ggplot(datap1, aes(y=ph1_spline1, x=Neg_Op1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con TPI1
ggplot(datap1, aes(y=ph1_spline1, x=TPI1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con ndvi1 
ggplot(datap1, aes(y=ph1_spline1, x=ndvi1, colour=clima1)) + 
  geom_point(aes(fill=clima1))


######################################################
#Miramos si hay diferencias entre los métodos de PH
#Variable PH1 con DEM1
clima1<-as.factor(datap1$clima1)
class(clima1)
ggplot(datap1, aes(y=ph2_sum, x=DEM1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#Variable PH con AnHill1
ggplot(datap1, aes(y=ph2_sum, x=AnHill1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#Variable PH con Slo1
ggplot(datap1, aes(y=ph2_sum, x=Slo1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Aspect
ggplot(datap1, aes(y=ph2_sum, x=Aspect1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Plan_curv1
ggplot(datap1, aes(y=ph2_sum, x=Plan_curv1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#Variable PH con prof_curv1
ggplot(datap1, aes(y=ph2_sum, x=prof_curv1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Conv_Ind1
ggplot(datap1, aes(y=ph2_sum, x=Conv_Ind1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Totcatchar1
ggplot(datap1, aes(y=ph1_spline1, x=TotCatchAr1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con TW1
ggplot(datap1, aes(y=ph2_sum, x=TWI1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con LSFactor1
ggplot(datap1, aes(y=ph2_sum, x=LSfactor1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con CHBL1
ggplot(datap1, aes(y=ph2_sum, x=CHBL1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con CND1
ggplot(datap1, aes(y=ph2_sum, x=CND1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con VD1
ggplot(datap1, aes(y=ph2_sum, x=VD1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con RSP1
ggplot(datap1, aes(y=ph2_sum, x=RSP1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con MRVBF1
ggplot(datap1, aes(y=ph2_sum, x=MRVBF1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con MRRTF1
ggplot(datap1, aes(y=ph2_sum, x=MRRTF1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con TRI1
ggplot(datap1, aes(y=ph2_sum, x=TRI1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con visSky1
ggplot(datap1, aes(y=ph2_sum, x=visSky1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con SkyVF1
ggplot(datap1, aes(y=ph2_sum, x=skyVF1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con Pos_op1
ggplot(datap1, aes(y=ph2_sum, x=Pos_Op1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con neg_op1
ggplot(datap1, aes(y=ph2_sum, x=Neg_Op1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con TPI1
ggplot(datap1, aes(y=ph2_sum, x=TPI1, colour=clima1)) + 
  geom_point(aes(fill=clima1))
#variable PH con ndvi1 
ggplot(datap1, aes(y=ph2_sum, x=ndvi1, colour=clima1)) + 
  geom_point(aes(fill=clima1))

##DIAGRAMAS DE DISPERSION CON RESPECTO AL tipo de relieve
treliev<-as.factor(datap1$tipo_relieve1)
class(treliev)
levels(treliev)
#Varriable relieve con DEM1
ggplot(datap1, aes(y=ph1_spline1, x=DEM1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#Variable PH con AnHill1
ggplot(datap1, aes(y=ph1_spline1, x=AnHill1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#Variable PH con Slo1
ggplot(datap1, aes(y=ph1_spline1, x=Slo1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con Aspect
ggplot(datap1, aes(y=ph1_spline1, x=Aspect1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con Plan_curv1
ggplot(datap1, aes(y=ph1_spline1, x=Plan_curv1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#Variable PH con prof_curv1
ggplot(datap1, aes(y=ph1_spline1, x=prof_curv1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con Conv_Ind1
ggplot(datap1, aes(y=ph1_spline1, x=Conv_Ind1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con TW1
ggplot(datap1, aes(y=ph1_spline1, x=TWI1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con LSFactor1
ggplot(datap1, aes(y=ph1_spline1, x=LSfactor1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con CHBL1
ggplot(datap1, aes(y=ph1_spline1, x=CHBL1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con CND1
ggplot(datap1, aes(y=ph1_spline1, x=CND1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con VD1
ggplot(datap1, aes(y=ph1_spline1, x=VD1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con RSP1
ggplot(datap1, aes(y=ph1_spline1, x=RSP1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con MRVBF1
ggplot(datap1, aes(y=ph1_spline1, x=MRVBF1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con MRRTF1
ggplot(datap1, aes(y=ph1_spline1, x=MRRTF1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con TRI1
ggplot(datap1, aes(y=ph1_spline1, x=TRI1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con visSky1
ggplot(datap1, aes(y=ph1_spline1, x=visSky1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con SkyVF1
ggplot(datap1, aes(y=ph1_spline1, x=skyVF1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con Pos_op1
ggplot(datap1, aes(y=ph1_spline1, x=Pos_Op1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con neg_op1
ggplot(datap1, aes(y=ph1_spline1, x=Neg_Op1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con TPI1
ggplot(datap1, aes(y=ph1_spline1, x=TPI1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve1))
#variable PH con ndvi1 
ggplot(datap1, aes(y=ph1_spline1, x=ndvi1, colour=tipo_relieve1)) + 
  geom_point(aes(fill=tipo_relieve))


################################
library("RColorBrewer")
display.brewer.all()
#variable PH con Totcatchar1
clima1<-as.factor(datap1$clima1)
ggplot(datap1) + 
  geom_point(aes(y=ph1_spline1, x=Slo1, color=treliev, 
                 size=clima1))
#################################

##ejemplo
ggplot(CPS1985, aes(experience,log(wage))) +
  geom_point(aes(color=gender)) +
  labs(title="Diagrama de dispersión",
       subtitle= "ScatterPlot",
       caption="Fuente: CPS1985 (paquete AER)",
       x="Experiencia (en años)",
       y="Salario (en logaritmo)") +
  scale_color_discrete("Género", labels=c("Hombre","Mujer"))



##Box plot(ph por tipo de relieve)
names(datap1)
#Box plot de ph1_spline, clima y DEM1

ggplot(data = datap1,aes(x =clima1, y =DEM1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y AnHill1
ggplot(data = datap1, aes(x =clima1, y =AnHill1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y Slo1
ggplot(data = datap1, aes(x =clima1, y =Slo1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y Aspect1
ggplot(data = datap1, aes(x =clima1, y =Aspect1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y Plan_Curv1
ggplot(data = datap1, aes(x =clima1, y =Plan_curv1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y Prof_curv1
ggplot(data = datap1, aes(x =clima1, y =prof_curv1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y conv_Ind1
ggplot(data = datap1,aes(x =clima1, y =Conv_Ind1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y TotCatChar1
ggplot(data = datap1, aes(x =clima1, y =TotCatchAr1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y TWI1
ggplot(data = datap1, aes(x =clima1, y =TWI1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y LsFactor1
ggplot(data = datap1, aes(x =clima1, y =LSfactor1)) +
  geom_boxplot(aes(fill=clima1)) 

#Box plot de ph1_spline, clima y CHBLI1
ggplot(data = datap1, aes(x =clima1, y =ph1_spline1)) +
geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y CND1
ggplot(data = datap1, aes(x =clima1, y =CND1)) +
geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y VD1
ggplot(data = datap1, aes(x =clima1, y =VD1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y RSP1
ggplot(data = datap1, aes(x =clima1, y =RSP1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y MRVBF1
ggplot(data = datap1, aes(x =clima1, y =MRVBF1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y MRRTF1
ggplot(data = datap1, aes(x=clima1, y =MRRTF1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y TRI1
ggplot(data = datap1, aes(x =clima1, y =TRI1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y visSky1
ggplot(data = datap1, aes(x =clima1, y =visSky1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y skyVF1
ggplot(data = datap1, aes(x =clima1, y =skyVF1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y Pos_Op1
ggplot(data = datap1, aes(x =clima1, y =Pos_Op1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y Neg_Op1
ggplot(data = datap1, aes(x =clima1, y =Neg_Op1)) +
  geom_boxplot(aes(fill=clima1)) 

##Box plot de ph1_spline, clima y VD1
ggplot(data = datap1, aes(x =clima1, y =VD1)) +
  geom_boxplot(aes(fill=clima1)) 
##Box plot de ph1_spline, clima 
ggplot(data = datap1, aes(x =clima1, y =ph1_spline1)) +
  geom_boxplot(aes(fill=clima1)) 

levels(clima1)
levels(treliev)

##mapas de calor
#dajamos solo las variables cuantitativas
names(datap1)
datap2 <- datap1[,-c(24,25)]
names(datap2)
#valor absoluto de las matriz de correlaciones
round(abs(cor(datap2)),3)
#las más oscuras son las variables más correlacionadas
heatmap(abs(cor(datap2)),Rowv = NA, Colv = NA, revC = TRUE)

library(coorplot)

