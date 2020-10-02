setwd("E:\\IGAC2020\\POLITICA_TIERRAS\\DATOS_RICARDO")

#========Separación en Hue Value y Chroma======
profiles <- data.frame(read_excel("BD_PERFILES_MODELAMIENTO_PT_2020.xlsx", sheet = "HORZ_V", na = "NA"))

names(profiles)
profiles$COLOR <- as.character(profiles$COLOR)
profiles$vc_H1 <- ifelse(grepl('2.5/',profiles$COLOR),yes=substr(profiles$COLOR, nchar(profiles$COLOR)-4, nchar(profiles$COLOR)),
                         no = substr(profiles$COLOR, nchar(profiles$COLOR)-2, nchar(profiles$COLOR)))
profiles$HUE_H1 <- substr(profiles$COLOR, 1,nchar(profiles$COLOR)-nchar(profiles$vc_H1))
profiles <- tidyr::separate(profiles,vc_H1,sep="/",into = c("V_H1","C_H1"))
profiles$V_H1 <- as.numeric(profiles$V_H1)
profiles$C_H1 <- as.numeric(profiles$C_H1)
profiles
write.csv2(profiles,"BDP_HOR_V.csv",row.names=F)
#==============