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
##' - 16 Nov 2016: update the file path to recompute the 
##' - 28 Fev 2017: change the path and use the new present day masks (binary proj within 250 km 
##'                bufferedconvex hull)
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
# setwd("J://People/Damien/BRISCA/workdir/")

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sp.id <- as.numeric(args[1])
# sp.id <- 61

## -- load needed packages ----------------------------------------------------- 
library(raster)

## on idiv
mod.dir <- "/data/idiv_sdiv/brisca/results/Biomod_pure_climate_final"
pres.day.filt <- "/data/idiv_sdiv/brisca/results/Present_day_masks"
out.dir <- "/work/georges/BRISCA/Biomod_biotic_interaction_maps_2017-02-28"
briscahub.dir <- "/home/georges/BRISCA/briscahub"

## on signe clust
# mod.dir <- "I://C_Write/Damien/BRISCA/backup_idiv_cluster/Biomod_pure_climate_filtered"
# out.dir <- "I://C_Write/Damien/BRISCA/Biomod_biotic_interaction_maps"
# briscahub.dir <- "J://People/Damien/BRISCA/briscahub"

dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)



# filt.pattern <- '_250kmBuffConvHull.grd'

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
sp.bio.inter <- sp.no.inter <- raster("/data/idiv_sdiv/brisca/results/raster_ref_27_02_2017.grd")

# sp_ <- sp.higher.bmnames[1]
# mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
# pattern <- '_filt_ch.grd'
# 
get.filt.prob.map <- function(sp_, mod.dir, pattern = '_filt_ch.grd'){
  ## get all the raster files matching the pattern
  ldir <- list.files(file.path(mod.dir, sp_), "proj_", full.names = TRUE)

  ##cat(ldir)
  prob.filt.files   <- sapply(ldir, function(x) list.files(file.path(x,"individual_projections"), pattern = paste0("EMca.*mergedData", pattern), full.names = TRUE))
  
  bin.filt.files  <- sapply(ldir, function(x) list.files(file.path(x,"individual_projections"), pattern = paste0("EMca.*mergedData_TSSbin", pattern), full.names = TRUE))
  prob.filt.stk <- raster::stack(prob.filt.files)
  bin.filt.stk <- raster::stack(bin.filt.files)
  probXbin.filt.stk <- prob.filt.stk * bin.filt.stk
  probXbin.filt.stk[is.na(probXbin.filt.stk)] <- 0 ## trick to be able to work on the full area
  probXbin.filt.stk <- sp.no.inter + probXbin.filt.stk
  return(probXbin.filt.stk)
}

## build the biotic interaction maps
for(disp_ in c("no", "min", "max")){
  cat("\n> dealing with", disp_, "dipersal limit.\n")
  sp.bio.inter <- sp.no.inter
  for(sp_ in sp.higher.bmnames){
    cat("add contib of :", which(sp.higher.bmnames == sp_), "/", length(sp.higher.bmnames), "\n")
    full.filt.pattern <- paste0("_filt_", disp_ ,"_disp", filt.pattern)
    sp_.compet.contrib <- get.filt.prob.map(sp_, mod.dir, 
                                            pattern = full.filt.pattern)
    sp.bio.inter <- sp.bio.inter + sp_.compet.contrib
  }
  ## update names of stk and save them on the hard drive
  stk.layer.names <- sub("/.*$", "", sub("^.*proj_pure_climat_", 
                                         "", list.files(file.path(mod.dir, sp.bmname), paste0("EMca.*mergedData", full.filt.pattern), recursive  = TRUE, full.names = TRUE)))
  names(sp.bio.inter) <- stk.layer.names
  writeRaster(sp.bio.inter, filename = file.path(out.dir, paste0(sp.bmname,"_bio_inter", full.filt.pattern)))
}


cat("\n done!")
q("no")

## end of script ---------------------------------------------------------------

## check that all projections have been filtered -------------------------------
rm(list = ls())
out.dir <- "/work/georges/BRISCA/Biomod_biotic_interaction_maps"
briscahub.dir <- "/home/georges/BRISCA/briscahub"
filt.pattern <- "_filt_no_disp_invdist.grd"
filt.pattern <- "_filt_min_disp_invdist.grd"
filt.pattern <- "_filt_max_disp_invdist.grd"

sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.bmname <- sp.tab$Biomod.name

(filt.files <- (list.files(out.dir, filt.pattern)))
which(!sapply(sp.bmname, function(sp_){any(grepl(sp_, filt.files))}))

## some graphical representations
out.dir.fig <- "/work/georges/BRISCA/Biomod_biotic_interaction_maps/fig"
dir.create(out.dir.fig, recursive = TRUE, showWarnings = FALSE)
library(rasterVis)
for(sp_ in sp.bmname){
  cat("\n", sp_)
  sp.biot.file <- grep(filt.pattern, list.files(out.dir, pattern = sp_, full.names = TRUE), value = TRUE)
  if(length(sp.biot.file)){
    sp.biot.stk <- raster::stack(sp.biot.file)
    crs(sp.biot.stk) <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 
    pdf(file.path(out.dir.fig, paste0(sp_, "_bio_inter", sub(".grd", ".pdf", filt.pattern))))
    levelplot(sp.biot.stk, main = sp_)
    dev.off()
  }
}
