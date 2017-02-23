##' ---
##' title: produce visual output to check that our models are behaving (more or less) well
##' date: 23/02/2017
##' author: damien g.
##' licence: GPL-3
##' ---

##' 
##' ## Short description
##' In this script we are producing the current projection ouptups maps of our pure climate models 
##' (CA ensemble model in contiunous and binary scale) ands overlay the convexhull. The idea is to 
##' visually check that our models fit more or less correctly the species distributions
##' 

##' 
##' ## The script 
##' 

setwd("/work/georges/BRISCA/Biomod_pure_climate_usgs_no_flaws")

path.to.briscahub <- "~/BRISCA/briscahub/"
path.to.convexhull <- "/data/idiv_sdiv/brisca/Data/january_session/Convex.hull/"

.libPaths("~/R/x86_64-pc-linux-gnu-library/3.2/")
library(raster)
library(rasterVis)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive


sp.list <- list.files()
# ## test
# sp_ <- "Amelanchier.alnifolia" 

pdf("~/CheckPureClimateProj.pdf")
for(sp_ in sp.list){
  cat("\n> deal with", sp_, "(", which(sp.list %in% sp_),"/", length(sp.list), ")")
  r <- raster(paste0(sp_, "/proj_pure_climat_current/individual_projections/", sp_,"_EMcaByTSS_mergedAlgo_mergedRun_mergedData.grd"))
  r_bin <- raster(paste0(sp_, "/proj_pure_climat_current/individual_projections/", sp_,"_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"))
  shp <- shapefile(file.path(path.to.convexhull, "Convex.hull", paste0(sp_, "_conv.hull.buf.shp")))
  shp_250m <- shapefile(file.path(path.to.convexhull, "Convex.buf", paste0(sp_, "_conv.hull.250.shp"))) 
  lp <- levelplot(stack(r/1000, r_bin), margin = FALSE, colorkey = FALSE, main = sp_) +
    layer(sp.lines(shp, lwd = 0.8, col = 'red')) +
    layer(sp.lines(shp_250m, lwd = 0.8, col = 'red', lty = 2))
  print(lp)
}
dev.off()


