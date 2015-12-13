################################################################################
##' @title create filters that will be use to constraint pure climate projections
##'
##' @author Damien G. 
##' @date 11/11/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build the 3 filters we will use to remove 
##'   unrealistics projected area obtained with the pure climate models.
##'   The filtered will be based on the binary version of ensemble models
##'   obtained with climate and inv distance. 
##'   We will build 3 filters:
##'     - no dispersal
##'     - 100 x min dispersal
##'     - 100 x max dispersal
##'     
##'   The dispersal disatnces come from TAME approach.
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
t.start <- Sys.time()

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sp.id <- as.numeric(args[1])

# ## test 
# sp.id <- 5

## ## -- load needed packages ----------------------------------------------------- 
library(raster)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


## -- define path to models and to output directories --------------------------
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_invdist"
out.dir <- "/work/georges/BRISCA/Biomod_dispersal_filters"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

disp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_red_tamme_disp.txt"),
                       sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# for(sp.id in 1:nrow(sp.tab)){ ## loop over species
cat("\n> sp:", sp.id, "/", nrow(sp.tab), "---------------------------------------------\n")

## get species attributes
sp.name <- sp.tab$Genus.species[sp.id]
sp.bmname <- sp.tab$Biomod.name[sp.id]

## get species dispersal distances attributes
## the max of the maximal dispersal distance confint x 100 years of moving 
sp.disp.max.dist <- 100 * 10^(disp.tab$log10MDD_uppCL[disp.tab$Biomod.name == sp.bmname])
## the min of the maximal dispersal distance confint x 100 years of moving
##' @note !!! this is not at all equivalent to a minimum dispersal disatance !!!
sp.disp.min.dist <- 100 * 10^(disp.tab$log10MDD_lwrCL[disp.tab$Biomod.name == sp.bmname])

## define the filenames of dispersal mask we want to save
fn.sp.disp.max <- file.path(out.dir, paste0(sp.bmname, "max_disp_mask_CA_invdist.grd"))
fn.sp.disp.min <- file.path(out.dir, paste0(sp.bmname, "min_disp_mask_CA_invdist.grd"))
fn.sp.disp.no <- file.path(out.dir, paste0(sp.bmname, "no_disp_mask_CA_invdist.grd"))

## load the reference mask dispersal 
mask.no.disp <- raster(file.path(mod.dir, sp.bmname, "proj_pure_climat_current", 
                                 "individual_projections", paste0(sp.bmname, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"))) 

## compute a mask where all non presence cells are set to NA
mask.no.disp.na <- reclassify(mask.no.disp, c(-Inf,0,NA))
mask.area <-  reclassify(mask.no.disp, c(-Inf,Inf,1))

## save the no disp mask
cat("\n> saving no disp mask...")
writeRaster(mask.no.disp, filename = fn.sp.disp.no, datatype = 'INT1S', overwrite = TRUE)

## compute the max disp buffer
mask.max.disp <- buffer(mask.no.disp.na, sp.disp.max.dist)
## reshape the mask in a correct format
mask.max.disp[is.na(mask.max.disp[])] <- 0
mask.max.disp <- mask.max.disp * mask.area 
## save the produced mask
cat("\n> saving max disp mask...")
writeRaster(mask.max.disp, filename = fn.sp.disp.max, datatype = 'INT1S', overwrite = TRUE)

## compute the min disp buffer
mask.min.disp <- buffer(mask.no.disp.na, sp.disp.min.dist)
## reshape the mask in a correct format
mask.min.disp[is.na(mask.min.disp[])] <- 0
mask.min.disp <- mask.min.disp * mask.area 
## save the produced mask
cat("\n> saving min disp mask...")
writeRaster(mask.min.disp, filename = fn.sp.disp.min, datatype = 'INT1S', overwrite = TRUE)

## done!
t.stop <- Sys.time()
cat("> completed on:")
print(t.stop)
cat("\n")
print(difftime(t.stop, t.start))

quit('no')
