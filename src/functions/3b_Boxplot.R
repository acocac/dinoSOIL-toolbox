##############################################################################
# title         : grid files (inputs) to extract fragstat and fractal metrics;
# purpose       : create separated grids (unit of analysis) from detection grid for fragstat and
#                 fractal analyses;
# producer      : preparado por Jonas Anderegg, ETH Zürich in 2019; Adaptado por Alejandro Coca-Castro
# last update   : in London, UK June 2015 / Updated in September 2015;
# inputs        : deforestation grid by year, fishnet (windows) shapefile;
# outputs       : split detection grid using  in GeoTIFF format (FRAGSTAT/FRACTAL INPUT);
# remarks 1     : licencia GNU General Public segun autor original;
###############################################################################

#Perform recursive feature elimination
boxplot_covars <- function(data, covar_group, covar_name){
  p <- ggplot(data[data$variable %in% (covar_group),], aes(variable, value, fill=factor(group))) +
    geom_boxplot(outlier.shape = NA) +
    #theme_ipsum_rc(axis_title_size = 16, base_size=13)
    labs(x=paste0('Covariables - ',covar_name), y='Valores normalizados [0-1]') +
    #scale_fill_manual(labels = names_group) +
    #scale_x_discrete(labels = etiquettes_p1) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    theme(legend.text=element_text(size=15), legend.title = element_blank(), legend.spacing.x = unit(1.0, 'cm')) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank()) +
    guides(fill = guide_legend(nrow = 2))

  return(p)
}


