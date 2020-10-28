##Analisis exploratorio, variable GRAN GRUPO###
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

##Selección de la variable interes, GRAN GRUPO
names(data)
data <- data[7] %>% na.omit
names(data)
View(data)


##Grafica de Gran Grupo##
#Se realiza una división en la grafica, entre frecuencias menores y mayores a 5# 

data$gran_grupo<-data$GRANGRUPO
frecuencias<-data%>%
  group_by(gran_grupo)%>%
  count()
frecuencias
frecuencias[,"diff"] = ifelse(frecuencias[,"n"]<5,"Menores","Mayores")


gran<-merge (data,frecuencias, by.x = "gran_grupo", by.y = "gran_grupo")

gran <- within(gran,gran_grupo <- factor(gran_grupo, 
                                         levels=names(sort(table(gran_grupo), 
                                                           decreasing=FALSE))))

gran$diff = factor(gran$diff, levels=c("Menores","Mayores"))

ggplot(data = gran, aes(x =gran_grupo, fill=gran_grupo)) + 
  geom_bar()+
  facet_grid(~diff, scales = "free", space = "free")+
  ggtitle("Histograma Gran Grupo")+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  labs(x="GRAN GRUPO", y="Frecuencia")+
  guides(fill=F)

levels(data$gran_grupo)#niveles de la variable GRan Grupo
