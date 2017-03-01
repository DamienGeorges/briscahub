##' ---
##' title: reproject all the formal pure climate models projections
##' author damien g.
##' date: 27/02/2017
##' ---

##' ## Short description
##' 
##' Because the projection system and the studied area have changed several time since the begining of 
##' project, we will here ensure that all the projection file we will use in the next step are all
##' in the exactly same coordiante projection system
##' 
##' ## Script
##' 

rm(list = ls())
setwd("/work/georges/BRISCA/")

## -- load needed packages ----------------------------------------------------- 
library(raster)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


## -- define path to models and to output directories --------------------------
mod.dir <- "/data/idiv_sdiv/brisca/results/Biomod_pure_climate_final" ## the directory where pure climate models are stored
ras.ref <- raster("/data/idiv_sdiv/brisca/results/raster_ref_27_02_2017.grd")

ras.list <- list.files(mod.dir, ".grd$", recursive = TRUE, full.names = TRUE)

for(fn_ in ras.list){
  cat(">", which(ras.list == fn_), "/", length(ras.list))
  r_ <- raster(fn_)
  method_ <- ifelse(grepl("_TSSbin.grd$", fn_), 'ngb', 'bilinear')
  projectRaster(r_, ras.ref, method = method_, filename = fn_, overwrite = TRUE)
}