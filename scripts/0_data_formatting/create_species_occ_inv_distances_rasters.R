##' ----------------------------------------------------------------------------
##' @title Create species distance rasters
##' @author damien g.
##' @date 26/10/2015
##' @licence GPL
##' ----------------------------------------------------------------------------

##' @note the XY rasters will be based on the ddeg raster we use for modelling
##'   our species disttributions

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sp.id <- as.numeric(args[1])
## sp.id <- 1

param.file <- "/home/georges/BRISCA/briscahub/data/sp.list_08102015_red.txt"
param.tab <- read.table(param.file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

sp.name <- param.tab$Genus.species[sp.id]

## lad libraries ---------------------------------------------------------------
require(rgdal)
require(raster)

## define the paths to the data and out dir ------------------------------------
in.gdd <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Current/Processed/Projected/tave10_esri"
in.occ <- "/data/idiv_sdiv/brisca/Data/Occurrence.tables.combined.all.sources.no.outliers.merged"
out.dir <- "/data/idiv_sdiv/brisca/Data/InvDistanceRaster"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## create the distance layers -----------------------------------------------

## define the projection system
proj <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 
## degree day
ddeg <- raster(file.path(in.gdd, "ddeg.grd"))
ddeg.non.na.cell <- which(!is.na(ddeg[]))
## sel all values to 0
ddeg[ddeg.non.na.cell] <- 0
## build the coordiates rasters
out.ras.sum <- out.ras.mean <- sp.ras <- ddeg
names(out.ras.sum) <- "sum_inv_dist"
names(out.ras.mean) <- "mean_inv_dist"
names(sp.ras) <- "sp.ras"

## laod species occurences
occ.dat <- read.table(file.path(in.occ, paste0(sp.name, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.cell <- unique(cellFromXY(sp.ras, occ.dat[, c('X', 'Y')]))
## put them into sp.ras
sp.ras[] <- NA
sp.ras[occ.cell] <- 1

## create a null raster template
ras.null <- out.ras.sum
ras.null[] <- NA


## calculate inv distance raster
for(cell.id in ddeg.non.na.cell){
  cell.num <- which(is.element(ddeg.non.na.cell, cell.id))
  if(cell.num %% 500 == 0) cat("\t", cell.num) 
  ras.dist <- ras.null
  ras.dist[cell.id] <- 1
  ras.dist <- 1 / distance(ras.dist)
  ras.dist <- ras.dist * sp.ras
  ras.dist[cell.id] <- sp.ras[cell.id]
  out.ras.sum[cell.id] <- cellStats(ras.dist, 'sum')
  out.ras.mean[cell.id] <- cellStats(ras.dist, 'mean')
  
  if(cell.num %% 5000 == 0){
    cat("\n writting the raster with", cell.num, "cells filled")
    old.ras <- list.files(out.dir, paste0("^.*_inv_dist_ras_", sp.name, ".*$"), full.names = TRUE)
    ## save the intermediate raster on the hard drive
    writeRaster(out.ras.sum, filename = file.path(out.dir, paste0("sum_inv_dist_ras_", sp.name, "_", cell.num, ".grd")),
                overwrite = TRUE)
    writeRaster(out.ras.mean, filename = file.path(out.dir, paste0("mean_inv_dist_ras_", sp.name, "_", cell.num, ".grd")),
                overwrite = TRUE)
    ## remove old rasters
    if(length(old.ras)) unlink(old.ras)
  }
}

old.ras <- list.files(out.dir, paste0("^.*_inv_dist_ras_", sp.name, ".*$"), full.names = TRUE)
## save the raster on the hard drive
writeRaster(out.ras.sum, filename = file.path(out.dir, paste0("sum_inv_dist_ras_", sp.name, ".grd")),
            overwrite = TRUE)
writeRaster(out.ras.mean, filename = file.path(out.dir, paste0("mean_inv_dist_ras_", sp.name, ".grd")),
            overwrite = TRUE)
## remove old rasters
if(length(old.ras)) unlink(old.ras)

quit("no")
## end of script -------------------------------------------------------------
