################################################################################
##' @title produce the present day masks based on our plure climate projections
##' and convex hull + 250km buffers
##'
##' @author Damien G. 
##' @date 23/02/2017
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build filtered vversion of pure climate 
##'   models projections. Here we will use convex hull to realise the filtering
##'   procedure
##'   
##' @log
##' 
##' @licencing GPL-3.
################################################################################

## -- init the script ----------------------------------------------------------
rm(list = ls())
setwd("/work/georges/BRISCA/")

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
sp.id <- as.numeric(args[1])

sp.list <- 


## -- load needed packages ----------------------------------------------------- 
library(raster)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


## -- define path to models and to output directories --------------------------
mod.dir <- "/data/idiv_sdiv/brisca/results/Biomod_pure_climate_final" ## the directory where pure climate models are stored
out.dir <- "/work/georges/BRISCA/Present_day_masks"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"
convex.hull.dir <- "/data/idiv_sdiv/brisca/Data/january_session/Convex.hull"

## get the species list
sp.list <- list.files(mod.dir)

for(sp_ in sp.list){
  cat("\n> deal with", sp_, "(", which(sp.list %in% sp_),"/", length(sp.list), ")")
  
  ## -- get the species projections we want to filter ----------------------------
  r_bin_file <- file.path(mod.dir, sp_, "proj_pure_climat_current", "individual_projections", paste0(sp_, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"))
  r_bin <- raster(r_bin_file)
  
  ## -- get the associated 250km buffered convex hull ----------------------------
  shp_file <- file.path(convex.hull.dir, "Convex.buf", paste0(sp_, "_conv.hull.250.shp"))
  shp <- shapefile(shp_file)
  
  ## -- ensure that r_bin is in the right projection system ----------------------
  r_bin <- projectRaster(r_bin, crs = crs(shp))
  
  ## -- do the filtering process -------------------------------------------------
  r_bin_masked <- mask(r_bin, shp, updatevalue = 0) * r_bin
  
  ## -- save the current mask on har drive ---------------------------------------
  writeRaster(r_bin_masked, filename = file.path(out.dir, paste0(sp_, "_present_day_mask.grd")), overwrite = TRUE)

}

cat("\n done!")
q("no")
## end of script ---------------------------------------------------------------
