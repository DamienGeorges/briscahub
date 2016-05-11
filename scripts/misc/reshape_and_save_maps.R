library(raster)

file.in <- "/data/idiv_sdiv/brisca/results/Biomod_pure_climate_filtered/Abies.balsamea/proj_pure_climat_current/individual_projections/Abies.balsamea_EMcaByTSS_mergedAlgo_mergedRun_mergedData_filt_ch.grd"

r_ <- raster(file.in)

ref.poly <- shapefile("~/BRISCA/data/arctic_shapefile/cp_coast_la.shp")
ref.poly@data$WATER <- factor(ref.poly@data$WATER, levels = c(0,1,2), labels = c("water", "arctic", "sub-arctic") )
ref.poly.arctic <- ref.poly[ref.poly@data$WATER %in% c("arctic", "sub-arctic"),]

## define the output file path and create the directory if needed 
f.out_ <- "~/BRISCA/workdir/test_reduced_raster.grd"
dir.create(dirname(f.out_), recursive = TRUE, showWarnings = FALSE)
## reproject the raster on shapefile system
r_ <- projectRaster(r_, crs = crs(ref.poly), filename = f.out_, overwrite = TRUE)
## mask it and crop it to shapefile extent
r_ <- crop(r_, ref.poly.arctic, filename = f.out_, overwrite = TRUE)
r_ <- mask(r_, ref.poly.arctic, filename = f.out_, overwrite = TRUE, datatype = "INT2U")

writeRaster(round(r_), filename = f.out_, overwrite = TRUE, datatype = "INT2U" )
# return(f.out_)

file.info(sub("grd", "gri", file.in))$size
file.info(sub("grd", "gri", f.out_))$size

system(paste0("du -sh ", sub("grd", "*", f.out_)))

r__ <- raster()
dataType(r__)
