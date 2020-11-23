# Title     : TODO
# Objective : TODO
# Created by: ac
# Created on: 10/7/20

data_sebas <- data.frame(read_excel('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/0_basededatos/BD_PERFILES_MODELAMIENTO_PT_2020_preprocesada.xlsx', sheet = 'HORZ_V', na = 'NA'))
data_tool <- read.csv('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/0_basededatos/BDP_cesarmagdalena_Vert.csv', sep=';')

sort(names(data_sebas)) == sort(names(data_tool))

all(data_sebas[,'C_H1'] == data_tool[,'C_H1'])

for (n in names(data_sebas)){
  cat(n)
  cat('\n')
  cat(all(data_sebas[,n] == data_tool[,n]))
  cat('\n')
}


datos_HOR_PERFILES <- data.frame(read_excel('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/0_basededatos/BD_PERFILES_MODELAMIENTO_PT_2020.xlsx', sheet = 'ORIGINAL', na = "N/A"), row.names='COD_PERFIL')
datos_HOR_OBSERVACIONES <- data.frame(read_excel('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/entrada/0_basededatos/BD_OBSERVACIONES_MODELAMIENTO_PT_2020.xlsx', sheet = 'ORIGINAL', na = "N/A"))

library(dplyr)

datos_HOR_OBSERVACIONES <- datos_HOR_OBSERVACIONES %>% distinct(.keep_all = TRUE)
x<- datos_HOR_OBSERVACIONES[,'COD_PERFIL']
length(x)


table(x)[table(x) > 1]

variables.usuario<-c('d','r','i')
as.list(seq(1,3,1), variables.usuario)

paste0(variables.usuario,collapse = seq_along(2))


matriz <- read.csv('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/datos/salida/1_covariables/tabular/MatrixRegresion.csv', sep=';')
class(matriz$pH.0_30_spline) = "numeric"
is(matriz[,'pH.0_30_spline'],'numeric')
names(matriz)

##rfe
#====================================================================================== -
file_path <- '/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/exploratorio/Rdata/pH-0_30_splinerfe_",c,"_",CV,"CV_variables.RData'

load(file_path)

out <- tidy_rfe_output(rfe, "RF")
rfe

formula <- as.formula(paste0("RF", ' ~ .'))

load('/Volumes/Alejo/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_cesarmagdalena/modelos/0_particion/pH-0_30_spline_particion.RData')

particion['train']