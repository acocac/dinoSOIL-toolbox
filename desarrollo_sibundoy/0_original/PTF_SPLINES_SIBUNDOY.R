

rm(list=ls())
install.packages("moments")
install.packages("olsrr")
install.packages("aspace",dependencies=T)
install.packages("rgdal",dependencies=T)
install.packages("sp",dependencies=T)
install.packages("readxl",dependencies=T)
install.packages("usdm",dependencies=T)
install.packages("raster",dependencies=T)
install.packages("GWmodel",dependencies=T)
install.packages("rgdal",dependencies=T)
install.packages("sp",dependencies=T)
install.packages("readxl",dependencies=T)
install.packages("mvoutlier",dependencies=T)
install.packages("MVN",dependencies=T)
install.packages("ade4",dependencies=T)
install.packages("spdep",dependencies=T)
install.packages("geoR",dependencies=T)
install.packages("gstat",dependencies=T)
install.packages("ape",dependencies=T)
install.packages("lattice",dependencies=T)
install.packages("spatial",dependencies=T)
install.packages("spdep",dependencies=T)
install.packages("geoR",dependencies=T)
install.packages("gstat",dependencies=T)
install.packages("SDMTools",dependencies=T)
install.packages("RColorBrewer",dependencies=T)
install.packages("GISTools",dependencies=T)
install.packages("agricolae",dependencies=T)
install.packages("car",dependencies=T)
install.packages("GISTools",dependencies=T)
install.packages("MASS",dependencies=T)             
install.packages("GSIF",dependencies=T)    
install.packages("aqp",dependencies=T)  
install.packages("hydroGOF",dependencies=T)
install.packages("plyr",dependencies=T)

library(gtools)
library(moments)
library(olsrr)
library(aspace)
library(rgdal)
library(sp)
library(readxl)
library(usdm)
library(raster)
library(GWmodel)
library(rgdal)
library(sp)
require(mvoutlier)
library(MVN)
library(ade4)
library(spdep)
library(geoR)
library(gstat)
library(ape)
library(lattice)
library(spatial)
library(spdep)
library(geoR)
library(gstat)
library(SDMTools)
library(RColorBrewer)
library(GISTools)
library(agricolae)
library(car)
library(GISTools)
library(MASS)
library(aqp)
library(GSIF)
library(nortest)
library(hydroGOF)
library(plyr)
library(raster)

setwd("E:\\SIBUNDOY\\Compilado Sibundoy\\7 BASE DE DATOS RESULTADOS ANALISIS DE LABORATORIO")


dir()
perfiles <- data.frame(read_excel("base_perfiles.xlsx", sheet = "Hoja1",na=""))
dim(perfiles)
summary(perfiles)
summary(perfiles$Tipo_Relie)
base <- data.frame(read_excel("Base de Datos Laboratorio Sibundoy.xlsx", sheet = "LAB"))
names(base)
dim(base)
summary(base)
names(perfiles)
base <- join(base,perfiles, by="Perfil",type="left")
dim(base)
names(base)
head(base)
write.csv(base,"base_lab_campo.csv",row.names = F)
summary(base)
View(base)
class(base)
#===========================================================================================
#FUNCIONES DE PEDOTRANSFERENCIA PARA ESTIMACI�N DE DENSIDAD APARENTE EN SIBUNDOY
#===========================================================================================

base <- data.frame(read_excel("SIBUNDOY_VF.xlsx", sheet = "BASE_DEF"))
str(base)
names(base)
dim(base)

dim(base)
base$Perfil <- factor(base$Perfil)
base$TXT_B <- factor(base$TXT_B)
base$TXT_P <- factor(base$TXT_P)
base$DA <- as.numeric(base$DA)
base$A_B <- as.numeric(base$A_B)
base$L_B <- as.numeric(base$L_B)
base$Ar_B <- as.numeric(base$Ar_B)
base$A_P <- as.numeric(base$A_P)
base$L_P <- as.numeric(base$L_P)
base$Ar_P <- as.numeric(base$Ar_P)
base$CO <- as.numeric(base$CO)
base$ORDEN <- factor(base$ORDEN)

###===========
#DESCRIPTIVA
###===========
library(moments)
descr <- function(columna){
	MAX <- max(columna,na.rm=T)
	MIN <- min(columna,na.rm=T)
	PROM <- mean(columna,na.rm=T)
	MED <- median(columna,na.rm=T)
	VAR <- var(columna,na.rm=T)
	DESVEST <- sd(columna,na.rm=T)
	CV <- DESVEST/PROM*100
	CURT <- kurtosis(columna,na.rm=T)
	SKEW <- skewness(columna,na.rm=T)
	return(data.frame(MAX, MIN, PROM,MED,VAR,DESVEST,CV, CURT, SKEW))
	}
names(base)
descr(base$DA)
descr(base$A_B)
descr(base$L_B)
descr(base$Ar_B)
descr(base$A_P)
descr(base$L_P)
descr(base$Ar_P)
descr(base$CO)


tapply(base$DA, base$ORDEN, mean,na.rm=T)
tapply(base$DA, base$ORDEN, median,na.rm=T)
tapply(base$DA, base$ORDEN, max,na.rm=T)
tapply(base$DA, base$ORDEN, min,na.rm=T)
tapply(base$DA, base$ORDEN, var,na.rm=T)
tapply(base$DA, base$ORDEN, sd,na.rm=T)



tapply(base$CO, base$ORDEN, mean,na.rm=T)
tapply(base$CO, base$ORDEN, median,na.rm=T)
tapply(base$CO, base$ORDEN, max,na.rm=T)
tapply(base$CO, base$ORDEN, min,na.rm=T)
tapply(base$CO, base$ORDEN, var,na.rm=T)
tapply(base$CO, base$ORDEN, sd,na.rm=T)
tapply(base$CO, base$ORDEN, length)


##==============
#MODELO GLOBAL
#===============
base <- subset(base, DA!="NA")

allrows <- 1:nrow(base)


set.seed(632)
cal1 <- sample(allrows, replace = F, size = 0.75*length(allrows))
val1 <- allrows[-cal1]


train <- base[cal1,]
test <- base[val1,]


names(base)
cor(train$DA, train[,c(8,9,10,17)],use="complete.obs")

library(nortest)
library(MASS)

dim(train)
mod1 <- lm(DA~CO, data=train)###Textura por byc
boxcox(mod1)
abline(v=0.42)
summary(mod1)
residuals(mod1)
shapiro.test(residuals(mod1))###NOrmalidad
lillie.test(residuals(mod1))###NOrmalidad
ad.test(residuals(mod1))###NOrmalidad
par(mfrow=c(2,2))
plot(mod1)
coef <- mod1$coefficients
coef
predict(mod1)###valores predichos
pred <- predict(mod1)###valores predichos
range(pred)
obs <- train$DA #valores observados
cor(pred,obs,)


names(test)
A_B <- test$A_B
CO <- test$CO
func <- expression(0.97353095+(-0.02524026*CO))
eval(func)####Predichos obtenidos con el modelo ajustado con el conjunto train utilizando los datos del conjunto test

install.packages("hydroGOF")
library(hydroGOF)
cor(eval(func),test$DA)
rmse(eval(func),test$DA)
mae(eval(func),test$DA)
me(eval(func),test$DA)
mse(eval(func),test$DA)


#============================
#MODELO PARA ORDENES DE SUELO
#============================


names(base)
dim(base)
levels(base$ORDEN)
ands <- subset(base, DA!="NA"&ORDEN=="ANDISOL")
dim(ands)
hists <- subset(base, DA!="NA"&ORDEN=="HISTOSOL")
dim(hists)
epts <- subset(base, DA!="NA"&ORDEN=="INCEPTISOL")
dim(epts)
ents <- subset(base, DA!="NA"&ORDEN=="ENTISOL")
dim(ents)


allrows <- 1:nrow(hists)


set.seed(632)
cal1 <- sample(allrows, replace = F, size = 0.75*length(allrows))
val1 <- allrows[-cal1]


train <- hists[cal1,]
test <- hists[val1,]


names(base)
cor(train$DA, train[,c(8,9,10,17)],use="complete.obs")

library(nortest)
library(MASS)

dim(train)
mod1 <- lm(log(DA)~CO, data=train)###Textura por byc
boxcox(mod1)
abline(v=0.42)
summary(mod1)
residuals(mod1)
shapiro.test(residuals(mod1))###NOrmalidad
lillie.test(residuals(mod1))###NOrmalidad
ad.test(residuals(mod1))###NOrmalidad
par(mfrow=c(2,2))
plot(mod1)
coef <- mod1$coefficients
coef
predict(mod1)###valores predichos
pred <- predict(mod1)###valores predichos
range(pred)
obs <- train$DA #valores observados
cor(pred,obs)


names(test)
A_B <- test$A_B
CO <- test$CO
func <- expression(-0.41664315+(-0.03873506*CO))
eval(func)####Predichos obtenidos con el modelo ajustado con el conjunto train utilizando los datos del conjunto test

install.packages("hydroGOF")
library(hydroGOF)
cor(exp(eval(func)),test$DA)
rmse(exp(eval(func)),test$DA)
mae(exp(eval(func)),test$DA)
me(exp(eval(func)),test$DA)
mse(exp(eval(func)),test$DA)



#=======================
#MODELO POR PROFUNDIDAD
#=======================
names(base)
dim(base)
base$PROFUND
base <- subset(base, DA!="NA"&PROFUND=="4")
dim(base)

allrows <- 1:nrow(base)


set.seed(21)
cal1 <- sample(allrows, replace = F, size = 0.75*length(allrows))
val1 <- allrows[-cal1]


train <- base[cal1,]
test <- base[val1,]


names(base)
cor(train$DA, train[,c(8,9,10,17)],use="complete.obs")

library(nortest)
library(MASS)

dim(train)
mod1 <- lm(DA~CO, data=train)###Textura por byc
boxcox(mod1)
abline(v=0.42)
summary(mod1)
residuals(mod1)
shapiro.test(residuals(mod1))###NOrmalidad
lillie.test(residuals(mod1))###NOrmalidad
ad.test(residuals(mod1))###NOrmalidad
par(mfrow=c(2,2))
plot(mod1)
coef <- mod1$coefficients
coef
predict(mod1)###valores predichos
pred <- predict(mod1)###valores predichos
range(pred)
obs <- train$DA #valores observados
cor(pred,obs)


names(test)
CO <- test$CO
coef
func <- expression(coef[1]+(coef[2]*CO))
eval(func)####Predichos obtenidos con el modelo ajustado con el conjunto train utilizando los datos del conjunto test

install.packages("hydroGOF")
library(hydroGOF)
cor(eval(func),test$DA)
rmse(eval(func),test$DA)
mae(eval(func),test$DA)
me(eval(func),test$DA)
mse(eval(func),test$DA)




#==========================
#FUNCIONES SPLINE SIBUNDOY
#==========================

library(GSIF)
library(aqp)
library(plyr)
library(sp)
library(readxl)
getwd()

list.files(".")
excel_sheets("SIBUNDOY_VF.xlsx")
base <- data.frame(read_excel("SIBUNDOY_VF.xlsx", sheet = "BASE_DEF"))
dim(base)
names(base)
prof1_paramo <- join(data.frame(base$ID_PERFIL,base$PERFIL, base$PROF_IN, base$PROF_FIN,base$CO,base$DA),data.frame(base$ID_PERFIL,base$W_1,base$N_1), type='inner')
## promote to SPC and plot
depths(prof1_paramo) <-  base.PERFIL~ base.PROF_IN + base.PROF_FIN
## fit MP spline by profile
str(prof1_paramo)
try(SOC <- mpspline(prof1_paramo, 'base.CO', d = t(c(0,30))))
try(BLD <- mpspline(prof1_paramo, 'base.DA', d = t(c(0,30))))

str(prof1_paramo)
dat_subset <- data.frame(PERFIL = prof1_paramo@site,CO=SOC$var.std[,1],DA=BLD$var.std[,1])
names(dat_subset) <- c("PERFIL","CO","DA")
head(dat_subset)
dim(dat_subset)
site <- data.frame(read_excel("SIBUNDOY_VF.xlsx", sheet = "PERFILES"))
dim(site)
dat_subset2 <- join(site,dat_subset,by="PERFIL", type="inner")
dim(dat_subset2)

write.csv(dat_subset2, "SPLINE_SIBUNDOY.csv", row.names=F)



TAXON <- data.frame(read_excel("SIBUNDOY_VF.xlsx", sheet = "PERFILES"))
dim(site)
COyDA <- data.frame(read_excel("SIBUNDOY_VF.xlsx", sheet = "SIBUNDOY_COS"))
STOCK <- join(TAXON,COyDA,by="PERFIL", type="inner")
dim(STOCK)

write.csv(STOCK, "STOCKCOS_SIB.csv", row.names=F)

