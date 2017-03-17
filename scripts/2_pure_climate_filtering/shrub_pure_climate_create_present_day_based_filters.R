################################################################################
##' @title create filters that will be use to constraint pure climate projections
##' and biotic interaction predictors
##'
##' @author Damien G. 
##' @date 03/03/2017
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build the 1 filter we will use to remove 
##'   unrealistics projected area obtained with the pure climate models.
##'   The filtered will be based on the present day mask constructed with
##'   shrub_present_day_mask_building.R. This masks are the binary projection
##'   of the species within the 250km buffered convexhull
##'    
##'   We will build a filters:
##'     - 100 x max dispersal
##'     
##'   The dispersal disatnces come from TAME approach.
##'   
##'   Even if a unique filter is build here we will then use 3 different ones:
##'    - the present day mask: will be use for the no dispersal scenario
##'    - the 100 x max dispersal: will stand for an max dispersal scenario
##'    - then we will also use the no dispersal limitation scenario
##'   
##' @log
##' 
##' @licencing GPL
##'     Copyright (C) 2017  Damien G.
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
in.dir <- "/work/georges/BRISCA/Present_day_masks_2017_03_17"
out.dir <- "/work/georges/BRISCA/Future_day_masks_2017_03_17/max_dispersal"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"
ras.ref.file <- "/data/idiv_sdiv/brisca/results/raster_ref_27_02_2017.grd"

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_03.03.2017.txt"),
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
# ## the min of the maximal dispersal distance confint x 100 years of moving
# ##' @note !!! this is not at all equivalent to a minimum dispersal disatance !!!
# sp.disp.min.dist <- 100 * 10^(disp.tab$log10MDD_lwrCL[disp.tab$Biomod.name == sp.bmname])

## define the filenames of dispersal mask we want to save
fn.sp.disp.no <- list.files(in.dir, pattern = paste0("^", sp.bmname, ".*.grd$"), full.names = TRUE)
fn.sp.disp.max <- file.path(out.dir, sub("present_day_mask", "future_day_max_disp_mask", basename(fn.sp.disp.no)))

## load the reference mask dispersal 
mask.no.disp <- raster(fn.sp.disp.no)

## compute a mask where all non presence cells are set to NA
mask.no.disp.na <- reclassify(mask.no.disp, c(-Inf,0,NA))
mask.area <-  raster(ras.ref.file)

## compute the max disp buffer
mask.max.disp <- buffer(mask.no.disp.na, sp.disp.max.dist)
## reshape the mask in a correct format
mask.max.disp[is.na(mask.max.disp[])] <- 0
mask.max.disp <- mask.max.disp * mask.area 
## save the produced mask
cat("\n> saving max disp mask...")
writeRaster(mask.max.disp, filename = fn.sp.disp.max, overwrite = TRUE)


## done!
t.stop <- Sys.time()
cat("> completed on:")
print(t.stop)
cat("\n")
print(difftime(t.stop, t.start))

quit('no')
