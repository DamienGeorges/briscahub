## create raster maps for filtering out unsuitable areas

setwd("~/Work/BRISCA/workdir/")
output.dir <- "../data/mask_raster_arctic_area_2016-08-22"
dir.create(output.dir, showWarnings = FALSE, recursive = TRUE)

ref.grid <- raster("../outputs/2016-08-18/SRC_baseline_alpha_and_turnover_stack/summaryStack__climate_and_biointer_filtered__high__maximal__cesm1_cam5__RCP_2.6.grd")
plot(ref.grid)

## load all needed shapefiles
library(rasterVis)
library(maptools)

s_buff <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/Buffer.shp"))
s_sa <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/Sub_Arctic.shp"))
s_la <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/Low_Arctic.shp"))
s_ha <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/High_Arctic.shp"))
s_coast <- shapefile("/home/georgeda/Work/BRISCA/data/cp_coast_la.shp")
## coastine need to be rotated
s_coast2 <- elide(s_coast, rotate = 180, center = c(0,0))
crs(s_coast2) <- crs(s_buff)
s_ice <- shapefile("/home/georgeda/Work/BRISCA/data/Perm-ice/permaice_glaciers.shp")
## ice layer needs to be rotated
s_ice2 <- elide(s_ice, rotate = 180, center = c(0,0))
crs(s_ice2) <- crs(s_buff)

s_coast2 <- crop(s_coast2, s_buff)
s_ice2 <- crop(s_ice2, s_buff)

## rasterize shapefiles
r_buff <- rasterize(s_buff, ref.grid, field = "Id")
r_sa <- rasterize(s_sa, ref.grid, field = "Id")
r_la <- rasterize(s_la, ref.grid, field = "Id")
r_ha <- rasterize(s_ha, ref.grid, field = "Id")
s_ice2@data$Id  <- 0
r_ice <- rasterize(s_ice2, ref.grid, field = "Id")

## visual checking
r_buff[!is.na(r_buff[])] <- 1
r_sa[!is.na(r_sa[])] <- 1
r_la[!is.na(r_la[])] <- 1
r_ha[!is.na(r_ha[])] <- 1
crs(r_sa) <- crs(r_la) <- crs(r_ha) <- crs(ref.grid)

## invert the ice raster because it is something to exclude
r_ice[!is.na(r_ice[])] <- 1
r_ice[is.na(r_ice[])] <- 0
r_ice[r_ice[] == 1] <- NA
plot(r_ice)

## set all land values to 1
ref.grid[!is.na(ref.grid[])] <- 1

## remove iced areas
ref.grid <- mask(ref.grid, r_ice)
writeRaster(ref.grid, file.path(output.dir, "mask_full_area_no_ice.grd"), overwrite = TRUE)

## remove all out of subarctic area
ref.from.sa <- mask(ref.grid, r_sa)
writeRaster(ref.from.sa, file.path(output.dir, "mask_from_subarctic_area_no_ice.grd"), overwrite = TRUE)
writeRaster(mask(ref.from.sa, r_la, inverse = TRUE), file.path(output.dir, "mask_subarctic_area_no_ice.grd"), overwrite = TRUE)

## remove all out of low arctic area
ref.from.la <- mask(ref.from.sa, r_la)
writeRaster(ref.from.la, file.path(output.dir, "mask_from_lowarctic_area_no_ice.grd"), overwrite = TRUE)
writeRaster(mask(ref.from.la, r_ha, inverse = TRUE), file.path(output.dir, "mask_lowarctic_area_no_ice.grd"), overwrite = TRUE)

## keep only high arctic
ref.ha <- mask(ref.from.la, r_ha)
writeRaster(ref.ha, file.path(output.dir, "mask_higharctic_area_no_ice.grd"), overwrite = TRUE)
