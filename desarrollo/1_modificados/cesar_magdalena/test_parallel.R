

fn = system.file('/proyecto_cesarmagdalena/datos/salida/1_covariables/raster/covariables.TIF', package = "rgdal")
obj <- rgdal::GDALinfo(fn)

fn <- stack(fn)