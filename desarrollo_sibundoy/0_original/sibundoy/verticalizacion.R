library(readxl)
getwd()
data <- data.frame(read_excel("OBSERVACIONES_20160603_VF.xls", sheet = "RESHAPE", na = "N/A"),row.names="Obs")
#View(data)
names(data)
dim(data)

indx <- gsub(".*_", "", names(data))
 
PRU <- setNames(
  lapply(split(colnames(data), indx), function(x) data[x]),
  paste('data', sort(unique(indx)), sep="_"))
PRU
names(PRU)
PRU$data_HORDIAG
gather(PRU$data_HORDIAG)
data.frame(ID_PERFIL_HOR=ID_PERFIL_HOR,gather(PRU$data_HORDIAG))
#install.packages("tidyr")
library(tidyr)

fin  <- data.frame(0)
for(i in 1:length(PRU)){
  temp <- gather(PRU[[i]])
  names(temp) <- c("HORIZONTE",strsplit(names(PRU[[i]])[1],"_")[[1]][2])
  fin <- data.frame(temp,fin)
}
fin$HORDIAG
ID_PERFIL_HOR <- paste(rep(row.names(data),ncol(PRU[[2]])),gl(ncol(PRU[[2]]),nrow(PRU[[1]])),sep="_")
ID_PERFIL_HOR
final <- data.frame(ID_PERFIL_HOR=ID_PERFIL_HOR,HORIZONTE=gl(6,402),fin)
final
head(final)
dim(final)
names(final)
remove <- seq(3,41,2)
final <- final[,-c(remove)]
write.csv(final,"final.csv",row.names=F)






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
