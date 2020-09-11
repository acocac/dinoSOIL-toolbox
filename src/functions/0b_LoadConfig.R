##############################################################################
# title         : grid files (inputs) to extract fragstat and fractal metrics;
# purpose       : create separated grids (unit of analysis) from detection grid for fragstat and 
#                 fractal analyses;
# producer      : prepared by A. Coca;
# last update   : in London, UK June 2015 / Updated in September 2015;
# inputs        : deforestation grid by year, fishnet (windows) shapefile;
# outputs       : split detection grid using  in GeoTIFF format (FRAGSTAT/FRACTAL INPUT);
# remarks 1     : detection grid must be in projected projection (i.e IGH or LAE);
###############################################################################

LoadConfig = function(x)
{ 
conf.list <- lapply(strsplit(readLines(x, warn=FALSE)," "), as.character)

#read target lines
root.index <- grep("*path",conf.list)
root.path = conf.list[[root.index]][[length(conf.list[[root.index]])]]

newlist = list(root.path)
return(newlist)
}

LoadConfig(conf.file)
