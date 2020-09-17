## Establecemos el directorio de trabajo

getwd()
# Para este ejercicio vamos a usar las librerias raster y car
# # Vamos a instalarlas y cargalas en memoria
# install.packages("raster")
#install.packages("car")
# install.packages("rgdal")

## Load required packages
library(sp)
library(raster)
library(car)
library(rgdal)
library(gstat)
library(lattice)
library(ggplot2)
library(caret)
library(reshape)
library(aplpack)
library(tcltk)
library(MASS)
require(MASS)
library(akima)
library(geoR)
library(lattice)
library(maptools)



### Preparamos los datos ####
dir()
# Cargamos los datos de los splines 
dat <- read.csv("RegMatrix_VF.csv")
names(dat)
dim(dat)
### Marcamos 


str(dat)
dat$Cobertura <- factor(dat$Cobertura)
dat$Paisaje <- factor(dat$Paisaje)
dat$Tipo_relieve <- factor(dat$Tipo_relieve)
dat$Forma_terreno <- factor(dat$Forma_terreno)
dat$Mat_parental <-  factor(dat$Mat_parental)
dat$Orden <- factor(dat$Orden)

#tabla.estadisticas.cont(dat$OCSKGM)

par(mfrow=c(1,3))
hist(dat$COS30CM, prob=T,main="COS 30 cm", xlab="Stock COS (t.ha^-1)",col="lightblue", ylab="Densidad")
lines(density(dat$COS30CM))
rug(dat$COS30CM,col="blue")
grid(10,10)
curve(dnorm(x,mean=mean(dat$COS30CM),sd=sd(dat$COS30CM)), col="darkblue",lwd=2,add=T,yaxt="n")
text(400,0.004,bquote("("*bar(x)* "="*.(round(mean(dat$COS30CM),2))*")"))
plot(ecdf(dat$COS30CM),ylab="Frecuencia",xlab="Stock COS (t.ha^-1)",main="COS 30 cm")
boxplot(dat$COS30CM,col="lightblue",main="COS 30 cm",ylab="Stock COS (t.ha^-1)")
points(mean(dat$COS30CM),col="red",pch=16)


# # Transform data
par(mfrow=c(1,3))
hist(log(dat$COS30CM), prob=T,main="COS 30 cm", xlab="Stock COS (t.ha^-1)",col="lightblue",breaks=100, ylab="Densidad")
lines(density(log(dat$COS30CM)))
rug(log(dat$COS30CM),col="blue")
grid(10,10)
curve(dnorm(x,mean=mean(log(dat$COS30CM)),sd=sd(log(dat$COS30CM))), col="darkblue",lwd=2,add=T,yaxt="n")
text(2,0.5,bquote("("*bar(x)* "="*.(round(mean(log(dat$COS30CM)),2))*")"))
plot(ecdf(log(dat$COS30CM)),ylab="Frecuencia",xlab="Stock COS (t.ha^-1)",main="COS 30 cm")
boxplot(dat$COS30CM,col="lightblue",main="COS 30 cm",ylab="Stock COS (t.ha^-1)")
points(mean(log(dat$COS30CM)),col="red",pch=16)
## Recreamos el objeto con la ubicacion de los puntos

dat_sp <- dat
coordinates(dat_sp) <- ~ W_1 + N_1

## Ajustamos un modelo de regresion lineal multiple
## Pruebas de modelos ####

### Analisis de correlacion
names(dat_sp@data)
str(dat_sp)
names(dat_sp@data[,c(3:21)])
COR <- cor(as.matrix(dat_sp@data[,3]), as.matrix(dat_sp@data[,c(4:21)]))
COR
x <- subset(melt(COR), value != 1 | value != NA)
x <- x[with(x, order(-abs(x$value))),]
#as.character(x$X2[1:10])
x[1:5,]

idx <- as.character(x$X2[c(1:5)])

dat2 <- dat[c('COS30CM', idx, 'N_1', 'W_1')]
names(dat2)
#idx2 <-  as.character(c(idx,'Aquents','Folists','Udands','Udepts','Ustepts','SUP_AGUA',
'A_HUM','VEG0','VEG_HERB','BOSQUES','CULT','A_HET','PASTOS','TERART'))


## Ahora, a diferencia del ejercicio 4, el modelo lo ajustamos solo con datos.model
## y no con todos los datos...!
dat2 <- dat2[dat2$COS30CM != 0,]
modelo.MLR <- lm(COS30CM ~ . -N_1-W_1, data = dat2) 

summary(modelo.MLR)
anova(modelo.MLR)


## Hacemos seleccion de variables por stepwise
modelo.MLR.step <- step(modelo.MLR, direction="both")

summary(modelo.MLR.step)
anova(modelo.MLR.step)


par(mfrow=c(2,2)) 
plot(modelo.MLR.step)
par(mfrow=c(1,1))

#Falta de multicolinealidad en las variables x: podemos comprobar esto mediante
#el calculo de los Factores de Inflacion de la Varianza (FIVs)
library(car)
#Variables problematicas tienen sqrt(FIV) > 2
vif(modelo.MLR.step)
sqrt(vif(modelo.MLR.step)) 
modelo.MLR.step <- update(modelo.MLR.step, . ~ . -  DEM_90 )
sqrt(vif(modelo.MLR.step))
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - tx1mod3a )
sqrt(vif(modelo.MLR.step))
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - inssre3a )
sqrt(vif(modelo.MLR.step))
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - twisre3a )
sqrt(vif(modelo.MLR.step))
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - TX4MOD3a )
sqrt(vif(modelo.MLR.step))
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - PEND_90)
sqrt(vif(modelo.MLR.step))
modelo.MLR.step <- update(modelo.MLR.step, . ~ . - px4wcl3a )
sqrt(vif(modelo.MLR.step))

#Vamos usar la prueba de Bonferroni para valores atipicos: 
outlierTest(modelo.MLR.step)
summary(modelo.MLR.step)

'Covariables_VF_Rasterizadas/COV_VF.tif'
'Covariables_VF_Rasterizadas/namesCovariables_VF.rds'

# Lo convertimos a un SpatialGridDataFrame
getwd()
dir()
library(raster)
install.packages("raster")

COV <- stack("COV_SIBU.tif")
namesCov<- readRDS('NAMES_COV_SIB.rds')
names(COV)
names(COV) <- namesCov
names(COV)

COV<- COV[[idx]]


# Project point data 
dat2_sp <- dat2
coordinates(dat2_sp) <- ~ W_1 + N_1
dat2_sp@proj4string <- COV@crs
proy <- CRS("+proj=longlat +datum=WGS84")
dat2_sp@proj4string <- proy

dat2_sp <- spTransform(dat2_sp, CRS("+init=epsg:32618"))

# project covariates to WGS84 UTM zone 18N Colombia
start <- Sys.time()
COV <- projectRaster(COV, crs = CRS("+init=epsg:32618"), method='ngb')

COV.sp <- as(COV, "SpatialGridDataFrame")

## Datos duplicados?
zerodist(dat_sp)


### RK model 
library(automap)
#library(automap)
#dat2_sp <- dat2_sp[dat2_sp$COS30CM != 0,]


OCS.krige <- autoKrige(formula = as.formula(modelo.MLR.step$call$formula), 
                       input_data = dat2_sp, 
                       new_data = COV.sp,
                       verbose = TRUE,
                       block = c(1000, 1000))


RKprediction <- raster(OCS.krige$krige_output[1])
RKpredsd <- raster(OCS.krige$krige_output[3])

summary(RKprediction)
summary(RKpredsd)
print(Sys.time() - start)

memory.size()
> memory.size(TRUE)
memory.limit(40000)


# Remove bad values
values(RKprediction )[values(RKprediction ) < 0]  <- NA
values(RKprediction )[values(RKprediction ) > 1000]  <- NA
values(RKpredsd)[values(RKpredsd ) > 100]  <- NA


summary(RKprediction)
summary(RKpredsd)
getwd()
# 
plot(RKprediction)
plot(RKpredsd)

## Save results
getwd()
writeRaster(RKprediction, filename = "RK_PRED_11072018.tif",overwrite=T)
writeRaster(RKpredsd, filename = "RK_PREDSD_11072018.tif",overwrite=T)


#libreria necesaria
library(raster)
#importas tu raster
r1 <- raster ('RESULTADOS_PROCESOS/KRIGING/COL_OCS_RK_VF_DIC.tif')
#haces la operacion
r2 <- r1 *10
#y guardas el raster generado
writeRaster(r2, 'RESULTADOS_PROCESOS/KRIGING/COL_OCS_RK_VF_DIC_TON_HA.tif')


#importas tu raster
r3 <- raster ('RESULTADOS_PROCESOS/KRIGING/COL_OCS_RKpredsd_VF_DIC.tif')
#haces la operacion
r4 <- r3 *10
#y guardas el raster generado
writeRaster(r4, 'RESULTADOS_PROCESOS/KRIGING/COL_OCS_RKpredsd_VF_DIC_TON_HA.tif')

plot(r2)
plot(r4)
#### Uncertainty estimation
# using LOO cross-validation

dat_sp = dat_sp[which(!duplicated(dat_sp@coords)), ]

OCS.krige.cv <- autoKrige.cv(formula = as.formula(modelo.MLR.step$call$formula), 
                             input_data = dat2_sp, nfold = 5)

summary(OCS.krige.cv)

str(RKprediction)
