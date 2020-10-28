##Analisis exploratorio, variable FAMILIA TEXTURAL###
#Carga de paquetes#
library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)

##Limpiar espacio de trabajo##
rm(list=ls())

##Carga de datos(matriz de regresion)##
setwd("C:/Users/usuario/Desktop/Patricia/IGAC/Proyectos IGAC/PROYECTO II/MAPEO DIGITAL/Maching Learning/Ultima matriz de regresion PH")

# Cargan de datos
data <- read.csv('MatrixDatos.csv', sep=',')#lee los datos
View(data)

##Selección de la variable interes, FAMILIA TEXTURAL
names(data)
data <- data[9] %>% na.omit
names(data)
View(data)
summary(data$FAMILIA_TE)

##Grafica de Gran Grupo##
#Se realiza una división en la grafica, entre frecuencias menores y mayores a 5# 

data$familia_textural<-data$FAMILIA_TE
frecuencias<-data%>%
  group_by(familia_textural)%>%
  count()
frecuencias
frecuencias[,"diff"] = ifelse(frecuencias[,"n"]<5,"Menores","Mayores")


gran<-merge (data,frecuencias, by.x = "familia_textural", by.y = "familia_textural")

gran <- within(gran,gran_grupo <- factor(familia_textural, 
                                         levels=names(sort(table(familia_textural), 
                                                           decreasing=FALSE))))

gran$diff = factor(gran$diff, levels=c("Menores","Mayores"))

ggplot(data = gran, aes(x =familia_textural, fill=familia_textural)) + 
  geom_bar()+
  facet_grid(~diff, scales = "free", space = "free")+
  ggtitle("Histograma Gran Grupo")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(x="FAMILIA TEXTURAL", y="Frecuencia")+
  guides(fill=F)

levels(data$familia_textural)#niveles de la variable GRan Grupo

#como todas las clases de FAMILIA TEXURAL son mayores a cinco, entonces se ordenan
#estas de manera creciente
##Grafica FAMILIA TEXTURAL#
data <- within(data,familia_textural <- factor(familia_textural, 
                                               levels=names(sort(table(familia_textural), 
                                                                 decreasing=FALSE))))

ggplot(data = data, aes(x =familia_textural, fill=familia_textural)) + 
  geom_bar(aes(fill=familia_textural))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(x="GRAN GRUPO", y="Frecuencia")+
  ggtitle("Histograma Gran Grupo")+
  guides(fill=F)

