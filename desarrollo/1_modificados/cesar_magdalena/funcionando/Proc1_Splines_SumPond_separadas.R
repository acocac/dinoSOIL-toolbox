##=====================================================================================
#Codigo necesario para calcular pH a los dos intervalos de profundidad deseados:
#Intervalo 1: 0 a 30 cm
#Intervalo 2: 30 a 100 cm
#Opcion de calculo 1: Splines (funciones de suavizado de area equivalente)
#Opcion de calculo 2: Suma ponderada (asumiendo espesor de horizontes como ponderador)
#======================================================================================
rm(list=ls()) ### TODO falta comparar sebastian vs mia
library(readxl)
library(GSIF)
library(aqp)
library(plyr)

source('/src/functions/2a_phPonderado.R')

##Carga de base de datos con observaciones y perfiles
data_obs <- read.csv('/proyecto_cesarmagdalena/datos/salida/0_basededatos/BDO_cesarmagdalena_Vert.csv', sep=',',, na='NA')
data_per <- read.csv('/proyecto_cesarmagdalena/datos/salida/0_basededatos/BDP_cesarmagdalena_Vert.csv', sep=',',, na='NA')
data <- rbind.fill(data_obs,data_per)

names(data) <- c("profileId","HorID","HorNO","top","bottom","pH","depth","NOM","HCL","CLTEX")

#preprocesamiento
data$pH = as.numeric(as.character(data$pH))
data <- data %>% filter_at(vars(top,bottom),all_vars(!is.na(.)))
data[which(data$pH > 11),'pH'] = data[which(data$pH > 11),'pH'] / 10

#crear dataset sitios
sites <- data.frame(profileId=unique(data$profileId))

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

# Intervalos
wSum_0_30 <- PonderadopH(data,0,30)
wSum_30_100 <- PonderadopH(data,30,100)

#-------------------------------
#RESULTADO FINAL DE PONDERACION 
#-------------------------------

dfs <- list(sites,wSum_0_30,wSum_30_100)
final <- join_all(dfs, "profileId")

write.table(final, '/proyecto_cesarmagdalena/datos/salida/0_basededatos/BD_pH_Spl_SumPond_mio.csv',
            row.names = F, sep=',')

##comparar
ph_sebas <- read.csv('/proyecto_cesarmagdalena/datos/salida/0_basededatos/BD_pH_Spl_SumPond.csv', sep=',', na='NA')
ph_mia <- read.csv('/proyecto_cesarmagdalena/datos/salida/0_basededatos/BD_pH_Spl_SumPond_mio.csv', sep=',', na='NA')

ph_sebas <- ph_sebas[complete.cases(ph_sebas), ]
ph_mia <- ph_mia[complete.cases(ph_mia), ]

par(mfrow=c(2,2))
{
  hist(ph_mia$pH.0_30_spline)
  hist(ph_mia$pH.30_100_spline)
  hist(ph_mia$pH.0_30_Sum.Pond)
  hist(ph_mia$pH.30_100_Sum.Pond)
}

par(mfrow=c(2,2))
{
  hist(ph_sebas$pH.0_30_spline)
  hist(ph_sebas$pH.30_100_spline)
  hist(ph_sebas$pH.0_30_Sum.Pond)
  hist(ph_sebas$pH.30_100_Sum.Pond)
}