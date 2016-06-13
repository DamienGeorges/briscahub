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
##'   models projections. Here we will use the binary models projections
##'   of models built with inverse distance matrix to filter the pure
##'   climate projections
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

session.to.filter = 'cab' ## 'pc': Pure climate or 'cab': Climate and biotic inter

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sp.id <- as.numeric(args[1])

# ## test 
# sp.id <- 114 

## -- load needed packages ----------------------------------------------------- 
library(raster)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


## -- define path to models and to output directories --------------------------

if(session.to.filter == 'pc'){
  ## Pure Climate session
  mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
  out.dir <- file.path("/work/georges/BRISCA/Biomod_pure_climate_filtered/")  
} else if(session.to.filter == 'cab'){
  ## Climate and Biotic interactions  
  mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer"
  out.dir <- file.path("/work/georges/BRISCA/Biomod_climate_and_biointer_filtered/")  
} 

filt.dir <- "/work/georges/BRISCA/Biomod_dispersal_filters"
lapply(out.dir, dir.create, recursive = TRUE, showWarnings = FALSE)

briscahub.dir <- "/home/georges/BRISCA/briscahub"

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
sp.proj.filt.dir <- unlist(lapply(out.dir, function(od){sub(mod.dir, od, unique(dirname(sp.proj.files)))}))
lapply(sp.proj.filt.dir, dir.create, recursive = TRUE, showWarnings =  FALSE)

## -- load the inverse distance based filters --------------------------------------
fn.sp.disp.no <- file.path(filt.dir, paste0(sp.bmname, "no_disp_mask_CA_invdist.grd"))
fn.sp.disp.min <- file.path(filt.dir, paste0(sp.bmname, "min_disp_mask_CA_invdist.grd"))
fn.sp.disp.max <- file.path(filt.dir, paste0(sp.bmname, "max_disp_mask_CA_invdist.grd"))

mask.no.disp <- raster(fn.sp.disp.no)
mask.min.disp <- raster(fn.sp.disp.min)
mask.max.disp <- raster(fn.sp.disp.max)

for(sp.proj.id in 1:length(sp.proj.files)){ ## loop over raster to filter
  cat(sp.proj.id, "\t")
  ## get the file we want to filter
  sp.proj.file <- sp.proj.files[sp.proj.id]
  sp.proj <- raster(sp.proj.file)
  ## do the filtering process
  sp.proj.filt.no.disp <- sp.proj * mask.no.disp
  sp.proj.filt.min.disp <- sp.proj * mask.min.disp
  sp.proj.filt.max.disp <- sp.proj * mask.max.disp
  
  ## save filteredprojections on the hardrive
  sp.proj.filt.no.disp.file <- sub(".grd$", "_filt_no_disp_invdist.grd", sub(mod.dir, out.dir, sp.proj.file))
  wr <- writeRaster(sp.proj.filt.no.disp, filename = sp.proj.filt.no.disp.file, overwrite = TRUE) 

  sp.proj.filt.min.disp.file <- sub(".grd$", "_filt_min_disp_invdist.grd", sub(mod.dir, out.dir, sp.proj.file))
  wr <- writeRaster(sp.proj.filt.min.disp, filename = sp.proj.filt.min.disp.file, overwrite = TRUE)
  
  sp.proj.filt.max.disp.file <- sub(".grd$", "_filt_max_disp_invdist.grd", sub(mod.dir, out.dir, sp.proj.file))
  wr <- writeRaster(sp.proj.filt.max.disp, filename = sp.proj.filt.max.disp.file, overwrite = TRUE) 
} ## end loop over raster to filter
cat("\n")
# } ## end loop over species
cat("\n done!")
print(date())
cat("\nfiltering done!", file = file.path(out.dir, sp.bmname, "jobstate.txt"))



q("no")
## end of script ---------------------------------------------------------------

## check that all projections have been filtered -------------------------------
rm(list = ls())
out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
briscahub.dir <- "/home/georges/BRISCA/briscahub"

sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.bmname <- sp.tab$Biomod.name[sp.tab$Growth.form.height == 'SHRUB']

sp.nb.filt.files <- sapply(sp.bmname, function(sp_){
  length(list.files(file.path(out.dir, sp_), "invdist.grd$", recursive = TRUE))
})

which(sp.nb.filt.files < 116)
