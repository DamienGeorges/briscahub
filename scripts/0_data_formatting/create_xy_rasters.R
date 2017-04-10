##' ----------------------------------------------------------------------------
##' @title Create a x and a y laea coordinates rasters for our study area
##' @author damien g.
##' @date 16/10/2015
##' @licence GPL
##' ----------------------------------------------------------------------------

##' @note the XY rasters will be based on the ddeg raster we use for modelling
##'   our species disttributions

## lad libraries ---------------------------------------------------------------
require(rgdal)
require(raster)

## define the paths to the data and out dir ------------------------------------
in.gdd <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Current/Processed/Projected/tave10_esri"
out.dir <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Current/Processed/Projected/xy_raster"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## create the coordinates layers -----------------------------------------------

## define the projection system
proj <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 
## degree day
ddeg <- raster(file.path(in.gdd, "ddeg.grd"))
ddeg.non.na.cell <- which(!is.na(ddeg[]))
## extract coordinates
xy.laea <- xyFromCell(ddeg, ddeg.non.na.cell)
## build the coordiates rasters
x.laea <- y.laea <- ddeg
x.laea[ddeg.non.na.cell] <- xy.laea[, 1]
names(x.laea) <- "x.laea"
y.laea[ddeg.non.na.cell] <- xy.laea[, 2]
names(y.laea) <- "y.laea"
## save the raster on the hard drive
writeRaster(x.laea, filename = file.path(out.dir, "x_laea.grd"), overwrite = TRUE)
writeRaster(y.laea, filename = file.path(out.dir, "y_laea.grd"), overwrite = TRUE)

## end of script -------------------------------------------------------------