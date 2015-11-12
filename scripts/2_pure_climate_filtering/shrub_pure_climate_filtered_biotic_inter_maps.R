################################################################################
##' @title produce the biotic interaction maps based on filtered pure climate 
##'   models outputs
##'
##' @author Damien G. 
##' @date 12/11/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build the competition variable we will the use in 
##'   our full model.
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
# sp.id <- 1

## -- load needed packages ----------------------------------------------------- 
library(raster)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


## -- define path to models and to output directories --------------------------
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
out.dir <- "/work/georges/BRISCA/Biomod_biotic_interaction_maps"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"

filt.pattern <- '_filt_ch.grd'

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# for(sp.id in 1:nrow(sp.tab)){ ## loop over species
cat("\n> sp:", sp.id, "/", nrow(sp.tab), "---------------------------------------------\n")

## get species attributes
sp.name <- sp.tab$Genus.species[sp.id]
sp.bmname <- sp.tab$Biomod.name[sp.id]
sp.height <- sp.tab$All.height.median[sp.id]

## get species competitors
sp.higher.bmnames <- sp.tab$Biomod.name[sp.tab$All.height.median > sp.height]

## define an empty default biotic interaction map
sp.bio.inter <- sp.no.inter <- raster("/data/idiv_sdiv/brisca/Data/no_interaction_mask.grd")

# sp_ <- sp.higher.bmnames[1]
# mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
# pattern <- '_filt_ch.grd'
# 
get.filt.prob.map <- function(sp_, mod.dir, pattern = '_filt_ch.grd'){
  ## get all the raster files matching the pattern
  prob.filt.files <- list.files(file.path(mod.dir, sp_), paste0("EMca.*mergedData", pattern), recursive  = TRUE, full.names = TRUE)
  bin.filt.files <- list.files(file.path(mod.dir, sp_), paste0("EMca.*mergedData_TSSbin", pattern), recursive  = TRUE, full.names = TRUE)
  prob.filt.stk <- raster::stack(prob.filt.files)
  bin.filt.stk <- raster::stack(bin.filt.files)
  probXbin.filt.stk <- prob.filt.stk * bin.filt.stk
  probXbin.filt.stk[is.na(probXbin.filt.stk)] <- 0 ## trick to be able to work on the full area
  return(sp.no.inter + probXbin.filt.stk)
}

## build the biotic interaction maps
for(sp_ in sp.higher.bmnames){
  cat("add contib of :", which(sp.higher.bmnames == sp_), "/", length(sp.higher.bmnames), "\n")
  sp.bio.inter <- sp.bio.inter + get.filt.prob.map(sp_, mod.dir, pattern = filt.pattern)
}

## rename the layers of the stack

## update names of stk and save them on the hard drive
stk.layer.names <- sub("/.*$", "", sub("^.*proj_pure_climat_", 
                                       "", list.files(file.path(mod.dir, sp.bmname), paste0("EMca.*mergedData", pattern), recursive  = TRUE, full.names = TRUE)))
names(sp.bio.inter) <- stk.layer.names
writeRaster(sp.bio.inter, filename = file.path(out.dir, paste0(sp.bmname,"_bio_inter", filt.pattern)))

cat("\n done!")
q("no")

## end of script ---------------------------------------------------------------

## check that all projections have been filtered -------------------------------
rm(list = ls())
out.dir <- "/work/georges/BRISCA/Biomod_biotic_interaction_maps"
briscahub.dir <- "/home/georges/BRISCA/briscahub"
filt.pattern <- "_filt_ch.grd"

sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.bmname <- sp.tab$Biomod.name

length(list.files(out.dir, filt.pattern))