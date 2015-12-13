################################################################################
##' @title produce pure climate map of heights
##'
##' @author Damien G. 
##' @date 11/11/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to produce the map of height over arctic based on 
##'   pure climate models
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

# ## retrieve input arguments ----------------------------------------------------
# args <- commandArgs(trailingOnly = TRUE)
# sp.id <- as.numeric(args[1])
# 

## -- load needed packages ----------------------------------------------------- 
library(raster)

rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


## -- define path to models and to output directories --------------------------
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final_summary_v2"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"
# convex.hull.dir <- "~/BRISCA/workdir/brsica_shrubs_convex_hull_full_filtered"


## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

for(sp.id in 1:nrow(sp.tab)){ ## loop over species
  cat("\n> sp:", sp.id, "/", nrow(sp.tab), "---------------------------------------------\n")
  
  sp.name <- sp.tab$Genus.species[sp.id]
  sp.bmname <- sp.tab$Biomod.name[sp.id]
  sp.height <- sp.tab$All.height.median[sp.id]
  
  ## -- get the species projections we want to use to produce height map -------
  
  ldir <- list.files(file.path(mod.dir, sp.bmname), "proj_", full.names = TRUE)
  ##' @note !!!!! TRICKY PART !!!!! => cause some csiro proj have failed.. we will remove them from this version of heigt maps
  ldir <- grep("csiro_mk360", ldir, value = TRUE, invert = TRUE)

  ##cat(ldir)
  sp.proj.files  <- sapply(ldir, function(x) list.files(file.path(x,"individual_projections"), pattern = "EMwmeanByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd", full.names = TRUE))
  sp.proj.files <- as.character(unlist(sp.proj.files)) 

  ##cat(sp.proj.files)  
  ## -- produce the height stack of this species -------------------------------
  sp.proj.stk <- raster::stack(sp.proj.files)
  sp.height.stk <- sp.proj.stk * sp.height
  ##print(sp.proj.stk)
  
  ## -- add this height stack to the current one -------------------------------
  if(sp.id == 1){
    alpha.div.stk <- sp.proj.stk
    height.sum.stk <- height.max.stk <- sp.height.stk
  } else {
    alpha.div.stk <- alpha.div.stk + sp.proj.stk
    height.sum.stk <- height.sum.stk + sp.height.stk
    height.max.stk <- stackApply(stack(height.max.stk, sp.height.stk), rep(1:nlayers(height.max.stk), 2), fun = max)
  }
}
## make a backup point here 
save.image(file.path(out.dir, "spchmc_backup.RData"))
# load(file.path(out.dir, "spchmc_backup.RData"))

## update names of stk and save them on the hard drive
stk.layer.names <- sub("/.*$", "", sub("^.*proj_pure_climat_", 
                       "", sp.proj.files))
names(alpha.div.stk) <- names(height.sum.stk) <- names(height.max.stk) <- stk.layer.names
writeRaster(alpha.div.stk, filename = file.path(out.dir, "pure_climate_alpha_div_stk.grd"))
writeRaster(height.sum.stk, filename = file.path(out.dir, "pure_climate_height_sum_stk.grd"))
writeRaster(height.max.stk, filename = file.path(out.dir, "pure_climate_height_max_stk.grd"))

cat("\n done!")
q("no")
## end of script ---------------------------------------------------------

