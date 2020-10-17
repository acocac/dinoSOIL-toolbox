##=====================================================================================
#Codigo necesario para calcular pH a los dos intervalos de profundidad deseados:
#Intervalo 1: 0 a 30 cm
#Intervalo 2: 30 a 100 cm
#Opcion de calculo 1: Splines (funciones de suavizado de area equivalente)
#Opcion de calculo 2: Suma ponderada (asumiendo espesor de horizontes como ponderador)
#======================================================================================
rm(list=ls())
library(readxl)
library(GSIF)
library(aqp)
library(plyr)

##Carga de base de datos con observaciones y perfiles
#data <- read.csv("E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\BASES\\BD_28092020.csv",sep=";",na="NA")
data <- data.frame(read_excel('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/0_basededatos/BD_28092020_PH.xlsx', sheet = 'BD_28092020', na = 'N/A'))

names(data) <- c("profileId","HorID","HorNO","top","bottom","pH","NOM","COH1","CLTEX")
data$pH = as.numeric(as.character(data$pH))
sites <- data.frame(profileId=unique(data$profileId))
names(sites)
str(data)
data <- data[data$profileId =="AF-006", ]

#--------------------------------------------------------
#OPCION 1: FUNCIONES DE SUAVIZADO DE AREA EQUIVALENTE
#--------------------------------------------------------

##Crear objeto SoilProfileCollection
dat_aqp <- data
names(dat_aqp)
depths(dat_aqp) <- profileId ~ top + bottom
site(dat_aqp) <- sites

# Hallar el valor deseado con splines
{
try(pH.0_30 <- mpspline(dat_aqp, 'pH', d = t(c(0,30))))
try(pH.30_100 <- mpspline(dat_aqp, 'pH', d = t(c(30,100))))
dat_spline <- data.frame(profileId = dat_aqp@site$profileId,
                         pH.0_30_spline = pH.0_30$var.std[,1],
                         pH.30_100_spline = pH.30_100$var.std[,1])
sites <- plyr::join(sites,dat_spline,by="profileId")
}


#-------------------------------------------------------------------------------
#OPCION 2: SUMA PONDERADA (ASUMIENDO EL ESPESOR DEL HORIZONTE COMO PONDERADOR)
#-------------------------------------------------------------------------------

# Intervalo deseado
upDepth <- 30
lowDepth <- 100

data2 <- data

# Nuevos rangos de prof inicial y final para cada horizonte
data2$upper <- ifelse(data2$top <= upDepth, yes = upDepth, no = data2$top)
data2$upper <- ifelse(data2$top >= lowDepth, yes = lowDepth, no = data2$upper)
data2$lower <- ifelse(data2$bottom >= lowDepth, yes = lowDepth, no = data2$bottom)
data2$lower <- ifelse(data2$bottom < upDepth, yes = upDepth, no = data2$lower)

# Nuevo espesor de cada horizonte
data2$depth <- data2$lower - data2$upper

# Descartar horizonte sin dato de pH
(no.inf.indx <- which(is.na(data2$pH)))
data2 <- data2[-no.inf.indx,]

# Descartar horizontes que no caen en el rango deseado
data2 <- data2[data2$depth!=0,]

# Ponderacion de valores de pH por horizonte
data2$weighted <- (data2$depth*data2$pH)/(lowDepth-upDepth)
head(data2)
dim(data2)

# Suma ponderada de valores de pH por perfil
wSum<- ddply(
  .data = data2,
  .variables = c("profileId"),
  .fun = function(x){
    sum(x$weighted)
  }
)

# Resultado final
colnames(wSum)[2] <- paste0("pH.",upDepth,"_",lowDepth,"_Sum.Pond")
wSum$profileId <- as.character(wSum$profileId)
wSum

#-------------------------------
#RESULTADO FINAL DE PONDERACION 
#-------------------------------

# Union resultado final y base de sitioS
sites <- plyr::join(sites, wSum,by="profileId")
head(sites)
tail(sites)
summary(sites)
write.csv(sites,"E:\\IGAC2020\\ENTREGA_FINAL_CONTRATO\\POLITICA_TIERRAS\\MODELOS_2020\\BASES\\BD_pH_Spl_SumPond.csv",
          row.names = F)

par(mfrow=c(2,2))
{
  hist(sites$pH.0_30_spline)
  hist(sites$ pH.30_100_spline)
  hist(sites$pH.0_30_Sum.Pond)
  hist(sites$pH.30_100_Sum.Pond)
}

