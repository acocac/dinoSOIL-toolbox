#############################################################################
# titulo        : Predicion de Taxonomia;
# proposito     : Modelar la taxonomia usando metodos de Aprendizaje de Maquinas;
# autor(es)     : Preparado por Sebastian Gutierrez (SG), IGAC-Agrologia; Adaptado por Alejandro Coca-Castro (ACC), IGAC-CIAF;
# actualizacion : Creado SG en Bogota, Colombia / Actualizado por ACC en Septiembre 2020;;
# entrada       : Base de datos original;
# salida        : Base de datos verticalizada;
# observaciones : ninguna;
##############################################################################

ExpRFE <- function(){
  # ------------------------------------------------------- #
  # Librerias y funciones
  # ------------------------------------------------------- #
  # Librerias
  pckg = c('compareGroups','caret','raster',
           'doParallel','dplyr','labelled',
            'ranger', 'tidyverse', 'reshape2',
            'hrbrthemes', 'ggpubr')

  usePackage <- function(p) {
    if (!is.element(p, installed.packages()[,1]))
      install.packages(p, dep = TRUE)
    require(p, character.only = TRUE)
  }

  lapply(pckg,usePackage)

  # Funciones
  r.dir <- gsub('\\\\', '/', r.dir)
  source(paste0(r.dir,'/functions/0b_LoadConfig.R'))
  source(paste0(r.dir,'/functions/3a_Outliers.R'))
  source(paste0(r.dir,'/functions/3b_Boxplot.R'))
  source(paste0(r.dir,'/functions/3c_RFE.R'))

  # ------------------------------------------------------- #
  # Cargar archivo de configuracion y componentes
  # ------------------------------------------------------- #
  # Cargar archivo configuracion
  conf.args <- LoadConfig(conf.file)

  # Cargar componentes relacionados con este script
  proyecto.directorio <- conf.args[[1]]
  modelos.proyecto <- conf.args[[2]]
  modelos.proyecto = sort(unlist(strsplit(modelos.proyecto,';')))

  # ------------------------------------------------------- #
  # Directorios de trabajo
  # ------------------------------------------------------- #
  # Declarar directorios
  exploratorio.rfe.rdata = paste0(proyecto.directorio,'/exploratorio/Rdata')
  dir.create(exploratorio.rfe.rdata, recursive = T, mode = "0777", showWarnings = F)
  exploratorio.rfe.figuras = paste0(proyecto.directorio,'/exploratorio/figuras')
  dir.create(exploratorio.rfe.figuras, recursive = T, mode = "0777", showWarnings = F)

  # Definir directorio de trabajo
  setwd(paste0(proyecto.directorio))

  # ------------------------------------------------------- #
  # Carga y preparacion de los datos
  # ------------------------------------------------------- #
  datos.entrada <- paste0(proyecto.directorio,'/datos/entrada/1_covariables')

  # Cargar matrix observaciones
  dat_subset <- read.csv(paste0(datos.entrada,'/color/tabular/RegMatrix_VF_observaciones.csv'),sep=',')

  dat_subset$orden <- factor(dat_subset$orden)
  names_group <- levels(dat_subset$orden)

  explanatory_exclude <- c(1:26,32,49,60,28)

  final_df = data.frame(group=dat_subset$orden, dat_subset[,-c(explanatory_exclude)])

  gooddata = computeOutliers(dat_subset[,-c(explanatory_exclude)], type = 'remove')
  good_df_q95 = final_df[gooddata,]

  df_wnoise = good_df_q95
  #remove variable(s) with ZeroVariance
  df_wnoise[,nearZeroVar(df_wnoise)] = NULL

  exploratorio.boxplot.archivo <- paste0(exploratorio.rfe.figuras,'/boxplots_variables.png')
  if (!file.exists(exploratorio.boxplot.archivo)){
    cat(paste0('El archivo boxplot de las variables para RFE NO existe, se va crear','\n'))
    ##charts###
    preProcValues <- preProcess(df_wnoise[-1], method = c("range"))
    trainTransformed <- predict(preProcValues, df_wnoise[-1])
    boxplot_df <- cbind(group=df_wnoise$group,trainTransformed)
    target_reshape <- melt(boxplot_df, id = c('group'), value.name = "value")

    explanatory_topograficas <- c('ANALYTICAL_HILLSHADING','ASPECT','CONVERGENCE_INDEX',
                                  'CROSS_SEC_CURVATURE', 'DEM_05', 'FLOW_ACCUMULATION',
                                  'LONGITUDINAL_CURVATURE', 'LS_FACTOR', 'RELATIVE_SLOPE_POSITION',
                                  'SLOPE','TOPOGRAPHIC_WETNESS_INDEX','VALLEY_DEPTH',
                                  'VERTICAL_DISTANCE_TO_CHANNEL_NETWORK')

    explanatory_clima <- c('PPT_CLIP','TEMP_CLIP')

    explanatory_vegetacion <- c('NDVI_SIBUNDOY_CLIP','Cobertura')

    explanatory_geomorfologia <- c('MAT_PARENTAL','T_Relieve', 'F_TERRENO')

    explanatory_quimicacolores <- c('RF_5m_PRED_DIP_1_CAJ_10092018', 'idw_b','idw_g','idw_r','OK_Luv','OK_v','OK_u','OK_L',
                             'idw_RI','OK_B','OK_G','OK_R')

    p1 <- boxplot_covars(target_reshape, explanatory_topograficas, 'Topografia')
    p2 <- boxplot_covars(target_reshape, explanatory_clima, 'Clima')
    p3 <- boxplot_covars(target_reshape, explanatory_vegetacion, 'Vegetacion')
    p4 <- boxplot_covars(target_reshape, explanatory_geomorfologia, 'Geomorfologia')
    p5 <- boxplot_covars(target_reshape, explanatory_quimicacolores, 'Quimica y Color')

    final_plot <- ggarrange(p1, p2, p3, p4, p5, legend = 'top', common.legend = TRUE, ncol=1, nrow=5)
    final_plot <- annotate_figure(final_plot, top = text_grob('Orden', size = 17, face = 'bold'))

    png(file = paste0(exploratorio.rfe.figuras,'/boxplots_variables.png'), width = 1700, height = 3200, res = 150)
    print(final_plot)
    dev.off()
  } else {
    cat(paste0('El archivo boxplot de las variables para RFE ya existe','\n'))
  }

  #RFE #TODO replace with traditional RFE or find error character...
  data <- df_wnoise
  cat(table(data$group)) #TODO check reduction number of groups
  cat('\n')

  classifiers <- c('multinom')
  CV = 10
  subsets <- c(33:1)

  for (c in classifiers){
    file_name <- paste0("rfe_",c,"_",CV,"CV_variables.RData")
    exploratorio.rfe.modelo <- paste0(exploratorio.rfe.rdata,'/',file_name)
    if (!file.exists(exploratorio.rfe.modelo)){
      cat(paste0('RFE para el modelo ',c,' NO existe','\n'))
      # Set up a cluster
      no_cores <- detectCores() - 1

      cl <- makeCluster(no_cores, type = "SOCK")    #create a cluster
      registerDoParallel(cl)                #register the cluster

      set.seed(40)

      # Perform recursive feature elimination
      rfe <- perform_rfe(response = "group", base_learner = c, type = "classification",
                         p = 0.8, times = CV,
                         subsets = subsets, data = data,
                         importance = "permutation",
                         num.trees = 100)



      save(rfe, file=exploratorio.rfe.modelo)

      stopCluster(cl = cl)
    } else {
      cat(paste0('RFE para el modelo ',c,' ya existe','\n'))
    }
  }

  #====================================================================================== -
  c <- 'ranger'
  CV <- 5
  exploratorio.rfe.rdata <- '/Users/ac/Documents/Consultancy/IGAC/projects/3_mapeosuelos/desarrollos/soil-toolbox/proyecto_sibundoy/exploratorio/Rdata'
  file_name <- paste0("rfe_",c,"_",CV,"CV_variables.RData")
  file_path <- paste0(exploratorio.rfe.rdata,'/',file_name)

  load(file_path)

  #data <- rfe
  #subsets <- data[[1]][[length(data[[1]])-1]]
  #ranks <- lapply(data, "[[", 1) %>%
  #  Reduce(function(dtf1, dtf2) full_join(dtf1, dtf2, by = "var"), .) %>%
  #  purrr::set_names(., c("var", paste("Resample", 1:length(data), sep = "")))
  #RMSEtrain <- lapply(data, "[[", 2) %>% lapply(., cbind, subsets) %>%
  #  lapply(., as_tibble) %>% Reduce(function(dtf1, dtf2) full_join(dtf1, dtf2, by = "subsets"), .) %>%
  #  dplyr::select(subsets, everything()) %>%
  #  purrr::set_names(c("subset_size", paste("Resample", 1:length(data), sep = "")))
  #Trainperf <- RMSEtrain %>%
  #  gather(resample, RMSE, contains("Resample")) %>%
  #  group_by(subset_size) %>%
  #  arrange(subset_size) %>%
  #  summarise_at(vars(RMSE), funs(mean, sd), na.rm = TRUE) %>%
  #  mutate(set = "Train")

  out <- tidy_rfe_output(rfe, "MLR")
  #PROF <- plot_perf_profile(out[[1]])
  #cat(PROF)
  #
  #p1_rf1 <- PROF
  #p2_rf2 <- PROF
  #
  #final_plot <- ggarrange(p1_rf1, p2_rf2, legend = "top", common.legend = TRUE, ncol=2, nrow=1)
  #
  #png(file = paste0(exploratorio.rfe.figuras,"/RFE_comparison_all.png"), width = 1700, height = 1200, res = 150)
  #print(final_plot)
  #dev.off()


}
