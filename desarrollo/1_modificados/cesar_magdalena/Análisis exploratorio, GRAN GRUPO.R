##Analisis exploratorio, variable GRAN GRUPO###
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
names(data)#nombre de las variables
nrow(data)


##Eliminación de variables que no son de interes
names(data)
data <- data[,-c(1:7,9:16)] %>% na.omit
data$ORDEN <- factor(data$ORDEN)
names(data)

#arreglo de variables
#se quitaron las variables clima y Closs_Dep

x1<-as.numeric(data$DEM)
x2<-as.numeric(data$AnHill)
x3<-as.numeric(data$Slo)
x4<-as.numeric(data$Aspect)
x5<-as.numeric(data$Plan_Curv)
x6<-as.numeric(data$Prof_Curv)
x7<-as.numeric(data$Conv_Ind)
x8<-as.numeric(data$TotCatchAr)
x9<-as.numeric(data$TWI)
x10<-as.numeric(data$LSfactor)
x11<-as.numeric(data$CHBL)
x12<-as.numeric(data$CND)
x13<-as.numeric(data$VD)
x14<-as.numeric(data$RSP)
x15<-as.numeric(data$MRVBF)
x16<-as.numeric(data$MRRTF)
x17<-as.numeric(data$TRI)
x18<-as.numeric(data$visSky)
x19<-as.numeric(data$SkyVF)
x20<-as.numeric(data$Pos_Op)
x21<-as.numeric(data$Neg_Op)
x22<-as.numeric(data$TPI)
x23<-as.numeric(data$tipo_relieve)
x24<-as.numeric(data$ndvi)
x25<-grangrupo<-data$GRANGRUPO


DEM<-x1
AnHill<-x2
Slo<-x3
Aspect<-x4
Plan_Curv<-x5
Prof_Curv<-x6
Conv_Ind<-x7
TotCatchAr<-x8
TWI<-x9
LSfactor<-x10
CHBL<-x11
CND<-x12
VD<-x13
RSP<-x14
MRVBF<-x15
MRRTF<-x16
TRI<-x17
visSky<-x18
SkyVF<-x19
Pos_Op<-x20
Neg_Op<-x21
TPI<-x22
tipo_relieve<-x23
ndvi<-x24
GRANGRUPO<-x25

data1<-data.frame(DEM, AnHill, Slo, Aspect, Plan_Curv, Prof_Curv, Conv_Ind,
                  TotCatchAr, TWI, LSfactor, CHBL, CND, VD, RSP, MRVBF,
                  MRRTF, TRI, visSky, SkyVF, Pos_Op, Neg_Op, TPI, 
                  tipo_relieve, ndvi, GRANGRUPO)

View(data1)
head(data1)
length(data1)
nrow(data1)


##Analisis factorial
#Matriz de correlaciones(dat1)
cor.dat=round(cor(data1[,1:24]),3)
cor.dat

#0,3(baja colinealidad), 0,5(colinealidad media), 0,7(colinealidad alta). 
#Supuesto de colinealidad
det(cor.dat)###se confirma que hay alta colinealidad entre variables
#cercano a 0, indica alta multicolinealidad entre las variables. 
#igual a 0 (matriz no singular). Supuesto de multicolinealidad test 
#de esfericidad de Bartlett busca contrastar la hipótesis nula de 
#que la matriz de correlaciones es igual a una matriz de identidad

library(psych)
cortest.bartlett(cor.dat,n=nrow(data1))
nrow(data1)

##Medida de suficiencia o adecuación de la muestra 
#0,90 > KMO Muy bueno 0,90 > KMO > 0,80 Bueno 0,80 > KMO > 0,70 
#Aceptable 0,70 > KMO > 0,60 Mediocre o regular 0,60 > KMO > 0,50 
#Malo 0,50 > KMO 
#Inaceptable o muy malo Autovalores y autovectores de la matriz de 
#covarianzas de la muestra
KMO(cor.dat)##0.78 es adecuado un AF

aucor=eigen(cor.dat)

#Porcentajes de variación explicada por cada componente
Prop.Var=aucor$values/sum(aucor$values)*100
cumProp.var=cumsum(aucor$values/sum(aucor$values)*100)
porc<-data.frame(Comp=1:24,Autovalor=round(aucor$values,3),Porc.Var=round(Prop.Var,3),Acum.Porc.Var=round(cumProp.var,3))
row.names(porc)

#Cálculos de Comunalidades Matris de Autovalores(egien)
mautov=matrix(diag(aucor$values),ncol=24,nrow=24)#matriz de valores propios

#Matriz de Cargas Factoriales
lamda=aucor$vectors%*%sqrt(mautov)
hi=lamda%*%t(lamda)##matriz cuadrado

#comunalidad
hi2=diag(hi)
hi2

#especificidad
him=lamda[,1:2]%*%t(lamda[,1:2])

#comunalidad##mirar si es la matriz reducida#
hi2m=diag(him)
hi2m

#Matriz de Comunalidades

data.frame(varible=names(data1[,1:24]),
           inicial=round(hi2,3),extraccio=round(hi2m,3))

#Varianza Total
sum(aucor$values)

#Gráfico de sedimentación
#La gráfica de sedimentación muestra el número del 
#componente principal versus su valor propio correspondiente. 
#La gráfica de sedimentación ordena los valores propios desde 
#el más grande hasta el más pequeño. Los valores propios de 
#la matriz de correlación son iguales a las varianzas de los 
#componentes principales.
plot(1:24,aucor$values,type="l",xlab="Componentes principales",
     ylab="autovalores")

data.frame(varible=names(data1[,1:24]),comp1=round(-lamda[,1],3),
           comp1=round(lamda[,6],3))



#Calculando la Matriz de Puntuaciones Factoriales
#F=bX(ojo para escribir la ecuación)
B=solve(cor.dat)%*%lamda

#Matriz de Puntuaciones Factoriales de componente
data.frame(varible=names(data1[,1:24]),Fac1=round(-B[,1],3),
           Fac2=round(B[,2],3),Fac3=round(B[,2],3))

#ESTANDARIZACIÓN DE VARIABLES
#scale(data1[1:24])
estgor=data.frame(scale(data1 [1:24]))
#Estimación factorial del individuo.
Fp=data.matrix (estgor)%*%(B)
head(Fp)

##Aplicación: Gráfico correlaciones factores y variables
#grafico con puntuaciones
library(ggplot2)
B1=data.frame(B)
ggplot(B1,aes(-B1[,1],B1[,2],B1[,3],label=rownames(B1)))+geom_point()+
  geom_text(vjust = 2)+
  xlab("Fact 1")+ylab("Fact 2")+geom_hline(yintercept=0,size=1)+
  geom_vline(xintercept=0,size=1)

##factoriales
grap.fact=data.frame(y1=-Fp[,1],y2=Fp[,2],y3=Fp[,3],
                     lab=grangrupo,grupo=data[,25])
ggplot(grap.fact,aes(y1,y2,y3,label=lab,color=grangrupo))+
  geom_point()+geom_text(vjust = 2)+
  xlab("Fact 1")+ylab("Fact 2")+geom_hline(yintercept=0,size=1)+
  geom_vline(xintercept=0,size=1)




#matriz de correlacion(variables cuantitativas)
library(psych)
cor(v_num)
names(v_num)
length(v_num)
pairs(v_num)
pairs.panels(v_num)
boxplot(v_num,col=(1:6),main="Boxplots Variables cualitativas")

##correlaciones, mirar cuales son las más altas
v_tot<-data1[,-c(25)]
cor(v_tot)
class(v_tot)

#Nuestros datos es mejor tenerlos en un data.frame
#numero de clases de la variable grangrupo
data.frame(data$GRANGRUPO)
data$GRANGRUPO
-length(levels(data$GRANGRUPO))
niv_GG<-summary(data$GRANGRUPO)
#barplot(niv_GG,col =(1:28),main="Distribución Gran Grupo y sus niveles")
#legend.text=levels(data$GRANGRUPO),
gran_grupo<-factor(data$GRANGRUPO)
Gran_Grupo<-barplot(data$GRANGRUPO,)
#hist(Gran_Grupo,main="Histograma Gran Grupo", col=("red"),xlab ="Gran Grupo",ylab = "Frecuencias")
#data<-data.frame(data)
#class(data)
#class(data$GRANGRUPO)

##VARIABLE DEM###
class(data$Dem)
summary(data$Dem)
Dem<-as.numeric(data$Dem)
summary(Dem)
class(Dem)
plot(Dem)
#########MIRAR######

##Gráfica de Gran Grupo##
library(ggplot2)
data <- within(data,gran_grupo <- factor(gran_grupo, 
                                      levels=names(sort(table(gran_grupo), 
                                                        decreasing=FALSE))))

ggplot(data = data, aes(x =gran_grupo, fill=gran_grupo)) + 
geom_bar(aes(fill=gran_grupo))+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
    labs(x="GRAN GRUPO", y="Frecuencia")+
  ggtitle("Histograma Gran Grupo")+
  guides(fill=F)
   
#ggplot(data = data, aes(x =gran_grupo, fill=Gran_Grupo)) + 
  #geom_bar(aes(fill=gran_grupo))+
  #theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  #labs(x="GRAN GRUPO", y="Frecuencia")+
  #guides(x=F)
###########################
#variables acm
v_acm1<-data[,-c(3,4,6,7,9:11,13:27)]
names(v_acm1)
length(v_acm1)
names(data)

#Analisis de correspondencias multiples
#library(FactoMineR)
#library(factoextra)

#v_factor<-data.frame(v_factor)

#res.mca=MCA(v_acm1, graph = T)
#eig.val <- get_eigenvalue(res.mca)
#eig.val
#fviz_screeplot(res.mca, addlabels = TRUE)

#fviz_mca_biplot(res.mca, 
#repel = TRUE, # Avoid text overlapping (slow if many point)
#                ggtheme = theme_minimal())

#fviz_mca_var(res.mca, choice = "mca.cor", 
#             repel = TRUE, # Avoid text overlapping (slow)
#             ggtheme = theme_minimal())

#fviz_mca_var(res.mca, 
#             repel = TRUE, # Avoid text overlapping (slow)
#             ggtheme = theme_minimal())

#fviz_mca_var(res.mca, axes = c( 1,2), col.var = "",
 #            gradient.cols = c("#bb0010", "#00e732", "#0417de",
 #                              "#e317cf","#ebeb10"), 
  #           repel = TRUE, # Avoid text overlapping
#             ggtheme = theme_minimal())


#plot(res.mca,invisible=c("var","quali.sup"),cex=0.7)
#plot(res.mca,invisible=c("ind","quali.sup"))
#plot(res.mca,invisible="quali.sup")
#plot(res.mca,invisible="ind")
#plot(res.mca,invisible=c("ind","var"))

#lapply(dimdesc(res.mca),lapply,signif,3)
#plotellipses(res.mca,keepvar=c("Subject","Gender",
                               "Clitic_gender_match"))







     

