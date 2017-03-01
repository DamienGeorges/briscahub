################################################################################
##' @title produce the filtered projection version of pure climate continuous 
##'   and binary outputs
##'
##' @author Damien G. 
##' @date 11/11/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build filtered vversion of pure climate 
##'   models projections. Here we will use convex hull to realise the filtering
##'   procedure
##'   
##' @log
##' 
##' @licencing GPL
##'     Copyright (C) 2015  Damien G.
##' 
##'     This program is free software: you can redistribute it and/or modify
##'     it under the terms of the GNU General Public License as published by
##'     the Free Software Foundation, either version 3 of the License, or
##'     (at your option) any later version.
##' 
##'     This program is distributed in the hope that it will be useful,
##'     but WITHOUT ANY WARRANTY; without even the implied warranty of
##'     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##'     GNU General Public License for more details.
##' 
##'     You should have received a copy of the GNU General Public License
##'     along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

## -- init the script ----------------------------------------------------------
rm(list = ls())
setwd("/work/georges/BRISCA/")

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sp.id <- as.numeric(args[1])


## -- load needed packages ----------------------------------------------------- 
library(raster)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


## -- define path to models and to output directories --------------------------
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"
convex.hull.dir <- "/data/idiv_sdiv/brisca/Data/january_session/Convex.hull"


## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# for(sp.id in 1:nrow(sp.tab)){ ## loop over species
  cat("\n> sp:", sp.id, "/", nrow(sp.tab), "---------------------------------------------\n")
  
  sp.name <- sp.tab$Genus.species[sp.id]
  sp.bmname <- sp.tab$Biomod.name[sp.id]
  
  ## -- get the species projections we want to filter ----------------------------
  sp.proj.files <- list.files(path = file.path(mod.dir, sp.bmname), 
                              pattern = "EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.*grd|EMcaByTSS_mergedAlgo_mergedRun_mergedData.*grd",
                              recursive = TRUE, full.names = TRUE)
  
  ## -- create a nice directory organisation -------------------------------------
  sp.proj.filt.dir <- sub(mod.dir, out.dir, unique(dirname(sp.proj.files)))
  lapply(sp.proj.filt.dir, dir.create, recursive = TRUE, showWarnings =  FALSE)
  
  ## -- load the convex hull to filter data --------------------------------------
  sp.conv.hull.poly <- get(load(file.path(convex.hull.dir, paste0(sp.name, "_convex_hull_poly_area_group.RData"))))
  ## convert the list of SpatialPoligons into a single SpatialPoligons object
  poly.list <- sapply(sp.conv.hull.poly, function(x) return(x@polygons[[1]]))
  for(i in 1:length(poly.list)) poly.list[[i]]@ID <- as.character(i)
  sp.conv.hull.poly.union <- SpatialPolygons(poly.list, 
                                             proj4string = CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  ##' @note
  ##' trick to restart from previous point => to be comment if you want to reprocess process everithing
  last.computed <- 0
  last.computed <- length(list.files(file.path(out.dir, sp.bmname), ".grd$", recursive = TRUE))
  
  for(sp.proj.id in max(1, last.computed - 1):length(sp.proj.files)){ ## loop over raster to filter
    cat(sp.proj.id, "\t")
    ## get the file we want to filter
    sp.proj.file <- sp.proj.files[sp.proj.id]
    sp.proj <- raster(sp.proj.file)
    ## do the filtering process
    sp.proj.filt <- mask(sp.proj, sp.conv.hull.poly.union)
    ## save filteredprojections on the hardrive
    sp.proj.filt.file <- sub(".grd$", "_filt_ch.grd", sub(mod.dir, out.dir, sp.proj.file))
    wr <- writeRaster(sp.proj.filt, filename = sp.proj.filt.file, overwrite = TRUE) 
  } ## end loop over raster to filter
  cat("\n")
# } ## end loop over species
cat("\n done!")
cat(date(), "\nfiltering done!", file = file.path(out.dir, sp.bmname, "jobstate.txt"))



q("no")
## end of script ---------------------------------------------------------------

## check that all projections have been filtered -------------------------------
rm(list = ls())
out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
briscahub.dir <- "/home/georges/BRISCA/briscahub"

sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.bmname <- sp.tab$Biomod.name

sp.nb.filt.files <- sapply(sp.bmname, function(sp_){
  length(list.files(file.path(out.dir, sp_), ".grd$", recursive = TRUE))
})

which(sp.nb.filt.files < 116)
