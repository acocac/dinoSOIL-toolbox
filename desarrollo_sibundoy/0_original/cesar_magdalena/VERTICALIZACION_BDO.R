setwd("E:\\IGAC2020\\POLITICA_TIERRAS\\DATOS_RICARDO")
library(readxl)
library(tidyr)
getwd()
dir()
data <- data.frame(read_excel("BD_OBSERVACIONES_MODELAMIENTO_PT_2020.xlsx", sheet = "HORZ_H", na = "N/A"),row.names="COD_PERFIL")
data$Obs[duplicated(data$Obs)]##Detectar identificadores de observaciones
View(data)
names(data)
dim(data)


indx <- gsub(".*_", "", names(data))

PRU <- setNames(
  lapply(split(colnames(data), indx), function(x) data[x]),
  paste('data', sort(unique(indx)), sep="_"))
PRU
names(PRU)
length(PRU)
#PRU$data_ASPECTOSPEDS1
#gather(PRU$data_ASPECTOSPEDS1)
#View(data.frame(ID_PERFIL_HOR=ID_PERFIL_HOR,gather(PRU$data_COLORMATRIZHUMEDO1)))
#install.packages("tidyr")
library(tidyr)

fin  <- data.frame(0)
for(i in 1:length(PRU)){
  temp <- gather(PRU[[i]])
  names(temp) <- c("HORIZONTE",strsplit(names(PRU[[i]])[1],"_")[[1]][2])
  fin <- data.frame(temp,fin)
}
#View(fin)

ID_PERFIL_HOR <- paste(rep(row.names(data),ncol(PRU[[2]])),gl(ncol(PRU[[2]]),nrow(PRU[[1]])),sep="_")
ID_PERFIL_HOR
length(ID_PERFIL_HOR)
####OJOOO.....la función gl de la siguiente línea necesita como segundo argumento 
#el número de perfiles u observaciones que tiene la base original
final <- data.frame(ID_PERFIL_HOR=ID_PERFIL_HOR,HORIZONTE=gl(6,1644),fin)
View(final)
head(final)
dim(final)
names(final)
remove <- seq(3,21,2)###EN ESTA FUNCIÓN seq SOLO SE DEBE AJUSTAR EL SEGUNDO PARÁMETRO Y COLOCAR LA CANTIDAD DE COLUMNAS QUE TENGA LA TABLA almacenada en el objeto "final"
final <- final[,-c(remove)]
names(final)
final$ID_PERFIL <- row.names(data)
final <- final[,c(12,1,2,3:11)]#Reordenando columnas

View(final)
write.csv2(final,"BDO_HOR_V.csv",row.names=F)

names(final)
c("ID_PERFIL_HOR","HORIZONTE",names(final)[c(15,10,11,18,8,26,25,13,24,9,28,3,17,12,4,27,16,20,23,22,21,14,5,
                                             6,7,19)])

final_ordenado <- final[,c(1,2,15,10,11,18,8,26,25,13,24,9,28,3,17,12,4,27,16,20,23,22,21,14,5,
                           6,7,19)]
names(final_ordenado)
write.csv2(final_ordenado,"BDO_HOR_V_ordenado.csv",row.names=F)

##=======CÓDIGO PARA GENERAR EL ID DE HORIZONTES CUANDO NO SE TIENE LOS MISMOS 6 HORIZONTES PARA TODOS LOS PERFILES O CAJUELAS DESCRITAS
freq <- table(rpois(100, 5))
attach(data.frame(freq))
id <- c()
for (i in 1:length(Freq)) {
  temp <- seq(1:Freq[i])
  id <- c(id,temp)
}
id

fin  <- data.frame(0)
for(i in 1:length(PRU)){
  temp <- verticalize(PRU[[i]])
  temp
  fin <- data.frame(temp,fin)
}
fin

ID_PERFIL_HOR <- paste(rep(row.names(data),ncol(PRU[[2]])),gl(ncol(PRU[[2]]),nrow(PRU[[1]])),sep="_")
ID_PERFIL_HOR
final <- data.frame(ID_PERFIL_HOR,fin)
names(final) <- c("ID_P_H","TOP","PROF","NOM","BOTTOM","COLOR","ESP")
final[,8]<-NULL










verticalize <- function(elem_list){
  property <- c(elem_list[,1],elem_list[,2],elem_list[,3])
  property
  return(property)
}

data_long <- gather(PRU$data_TOP)
data_long
names(data_long) <- c("HORIZONTE",strsplit(names(PRU$data_TOP)[1],"_")[[1]][2])
names(data_long)


df <- data.frame(matrix(rnorm(9*9), ncol=9))
names(df) <- c("a_1", "b_1", "c_1", "a_p", "b_p", "c_p", "a_o1", "b_o1", "c_o1")
row.names(df) <- names(df)
indx <- gsub(".*_", "", names(data))
I can split the dataframe by the index that is given in the column.names after the underscore "_".
PRU <- setNames(
  lapply(split(colnames(data), indx), function(x) data[x]),
  paste('df', sort(unique(indx)), sep="_"))
PRU
str(PRU)
row.names(PRU$df_o1)
PRU$df_o1$ID <- row.names(PRU$df_o1)

PRU$df_p$ID <- row.names(PRU$df_p)
library(plyr)
join(PRU$df_o1,PRU$df_p,by="ID")
