################################################################################
##' @title produce summary graphs to try to highligth our study fundings
##'
##' @author Damien G. 
##' @date 12/10/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build some visual tools to help us to get an
##'   idea of what we can say about our modelling framework 
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
cat("\njob started on:", strftime(t.start, "%m/%d/%y - %H:%M:%S"))

## get the index we want to work with as script input
args <- commandArgs(trailingOnly = TRUE)
job.id <- as.numeric(args[1])

# ## test
# job.id <- 1
# ## end test

## define what type of maps we want to produce
work.with.bin <- TRUE
do.div.map <- TRUE
do.height.map <- TRUE
do.dispersal.map <- TRUE

## -- load needed packages ----------------------------------------------------- 
#library(biomod2, lib.loc = "~/R/biomod2_pkg/biomod2_3.1-73-04")
library(raster)
#library(grid)
#library(gridExtra)
library(rasterVis)

rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive

## -- define path to models and to output directories --------------------------
# session.id = 'pc'
# session.id = 'pcfno'
# session.id = 'pcfmax'
# session.id = 'cabno' ## here
# session.id = 'cabmax' ## and here
# session.id = 'cabnofno'
# session.id = 'cabnofmax'
# session.id = 'cabmaxfno'
# session.id = 'cabmaxfmax'


if(job.id == 1){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
  pc.pattern <- 'pc'
  pc.pattern.extra <- ''
  pc.dir.pattern.extra <- ''
  pc.main <- 'Pure Climate'
} else if(job.id == 2){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
  pc.pattern <- 'pcfno'
  pc.pattern.extra <- '_filt_no_disp_invdist'
  pc.dir.pattern.extra <- ''
  pc.main <- 'Climate + No Dispersal'
} else if(job.id == 3){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
  pc.pattern <- 'pcfmax'
  pc.pattern.extra <- '_filt_max_disp_invdist'
  pc.dir.pattern.extra <- ''
  pc.main <- 'Climate + Max Dispersal'
} else if(job.id == 4){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer"
  pc.pattern <- 'cabno'
  pc.pattern.extra <- ''
  pc.dir.pattern.extra <- '_no_disp_invdist'
  pc.main <- 'Climate + Biotic (min) '
} else if(job.id == 5){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer"
  pc.pattern <- 'cabmax'
  pc.pattern.extra <- ''
  pc.dir.pattern.extra <- '_max_disp_invdist'
  pc.main <- 'Climate + Biotic (max)'
} else if(job.id == 6){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_filtered"
  pc.pattern <- 'cabnofno'
  pc.pattern.extra <- '_filt_no_disp_invdist'
  pc.dir.pattern.extra <- '_no_disp_invdist'  
  pc.main <- 'Climate + Biotic (min) + No Dispersal'
} else if(job.id == 7){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_filtered"
  pc.pattern <- 'cabnofmax'
  pc.pattern.extra <- '_filt_max_disp_invdist'
  pc.dir.pattern.extra <- '_no_disp_invdist'  
  pc.main <- 'Climate + Biotic (min) + Max Dispersal'
} else if(job.id == 8){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_filtered"
  pc.pattern <- 'cabmaxfno'
  pc.pattern.extra <- '_filt_no_disp_invdist'
  pc.dir.pattern.extra <- '_max_disp_invdist'
  pc.main <- 'Climate + Biotic (max) + No Dispersal'
} else if(job.id == 9){
  pc.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_filtered"
  pc.pattern <- 'cabmaxfmax'
  pc.pattern.extra <- '_filt_max_disp_invdist'
  pc.dir.pattern.extra <- '_max_disp_invdist'
  pc.main <- 'Climate + Biotic (max) + Max Dispersal'
} else {
  stop("invalid session.id given")
} 

## define the binary pattern
bin.pattern <- ifelse(work.with.bin, "_bin", "_cont") 

## get the number of cpu available
slots = as.integer(Sys.getenv("NSLOTS", "1"))

cat("\nParams:")
cat("\n> job.id:", job.id)
cat("\n> nb cores required:", slots)
cat("\n> pc.mod.dir:", pc.mod.dir)
cat("\n> pc.pattern:", pc.pattern)
cat("\n> pc.main:", pc.main)
cat("\n> work.with.bin:", work.with.bin)

out.dir.figs <- "/work/georges/BRISCA/Biomod_framework_summary/figs"
out.dir.ras <- "/work/georges/BRISCA/Biomod_framework_summary/summary_raster"
dir.create(out.dir.figs, recursive = TRUE, showWarnings = FALSE)
dir.create(out.dir.ras, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"

## -- load th species list -----------------------------------------------------
cat("\nLoad sp.tab and land_mask...")
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

shrub.list <- sp.tab$Biomod.name[ sp.tab$Growth.form.height == 'SHRUB' ]

land_mask <- raster("/data/idiv_sdiv/brisca/Data/no_interaction_mask.grd")
fut_mask <- subset(raster("/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Future/CIAT_AR5_bio_prec_tmean_tmax_tmin/Processed/Projected_polar_laea_10km/Full_arctic_30_north/RCP_2.6_2080/cesm1_cam5/bio/bioproj_multi.grd"), 1) > 0
land_mask <- land_mask * fut_mask

## 0. Create EF stack maps  ----------------------------------------------------

cat("\nCurrent EF map stack creation...")
pc.shrub.cur.ras.files <- file.path(pc.mod.dir, shrub.list, paste0("proj_pure_climat_current", pc.dir.pattern.extra), 
  "individual_projections", paste0(shrub.list, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData", ifelse(work.with.bin, "_TSSbin", ""),  pc.pattern.extra,".grd"))
pc.shrub.cur.stk <- stack(pc.shrub.cur.ras.files)
if(!work.with.bin) pc.shrub.cur.stk <- pc.shrub.cur.stk / 1000

cat("\nFuture EF consensus map stack creation...")
pc.shrub.fut.ras.files <- lapply(shrub.list, function(sl){
  fut.proj.dir <- dir(file.path(pc.mod.dir, sl), 
                      pattern = paste0("proj_pure_climat_RCP.*", pc.dir.pattern.extra), full.names = TRUE)
  return(file.path(fut.proj.dir, 
                   "individual_projections", 
                   paste0(sl, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData", ifelse(work.with.bin, "_TSSbin", ""), pc.pattern.extra,".grd")))
})

# ## sequential version
# pc.shrub.fut.stk.lst <- lapply(pc.shrub.fut.ras.files, function(rf){
#   cat("* ")
#   pc.ca.mean <- mean(stack(rf))
#   writeRaster(pc.ca.mean, filename = file.path(out.dir.ras, sub("_.*", "pure_cllimate_mean_of_all_fut_CAbin.grd", basename(rf[1]))), overwrite = TRUE)
#   pc.ca.maj <- pc.ca.mean >= 0.5
#   return(pc.ca.maj)
# })

## paralel version
# library(foreach)
# library(doParallel, lib.loc = "/gpfs0/global/local/R/3.1.2-3/lib64/R/library")
# cl <- makeCluster(slots)
# registerDoParallel(cl)
# ##pc.shrub.fut.stk.lst <- vector("list", length(shrub.list))
# pc.shrub.fut.stk.lst <- foreach(k = 1:length(pc.shrub.fut.ras.files), .packages = c('raster')) %dopar% {
#   rf <- pc.shrub.fut.ras.files[[k]]
#   pc.ca.mean <- mean(stack(rf))
#   writeRaster(pc.ca.mean, filename = file.path(out.dir.ras, sub("_.*", paste0("_", pc.pattern, "_mean_of_all_fut_CAbin.grd"), basename(rf[1]))), overwrite = TRUE)
#   pc.ca.maj <- pc.ca.mean >= 0.5
# }

library(parallel)

##pc.shrub.fut.stk.lst <- vector("list", length(shrub.list))
pc.shrub.fut.stk.lst <- mclapply(1:length(pc.shrub.fut.ras.files), function(k){
  rf <- pc.shrub.fut.ras.files[[k]]
  pc.ca.mean <- mean(stack(rf))
  writeRaster(pc.ca.mean, filename = file.path(out.dir.ras, sub("_.*", paste0("_", pc.pattern, bin.pattern, "_mean_of_all_fut_CA.grd"), basename(rf[1]))), overwrite = TRUE)
  if(work.with.bin) pc.ca.maj <- pc.ca.mean >= 0.5 else pc.ca.maj <- pc.ca.mean
  return(pc.ca.maj)
}, mc.cores = slots)

pc.shrub.fut.stk <- stack(pc.shrub.fut.stk.lst)
if(!work.with.bin) pc.shrub.fut.stk <- pc.shrub.fut.stk / 1000
# ## or reload from hard drive

## 1. create a diversity map and a diversity map change ------------------------
#if(do.div.map){
  cat("\nCurrent Diversity calculation...")
  pc.div.cur <- sum(pc.shrub.cur.stk, na.rm = TRUE)
  pc.div.cur <- pc.div.cur + land_mask
  writeRaster(pc.div.cur, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_current_alpha_div.grd")),  overwrite = TRUE)
  
  cat("\nFuture Diversity calculation...")
  pc.div.fut <- sum(pc.shrub.fut.stk, na.rm = TRUE)
  pc.div.fut <- pc.div.fut + land_mask
  writeRaster(pc.div.fut, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_futConsensus_alpha_div.grd")), overwrite = TRUE)
  
  cat("\nDiversity Change calculation...")
  pc.div.change <- pc.div.fut - pc.div.cur
  
  
  cat("\nProduce Diversity maps...")
  ## define a custom color theme for the raster
  ras.theme.div <- rasterTheme(region = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
  ras.at.div <- seq(0, 100, 2)
  
  ras.theme.div.change <- rasterTheme(region = c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
  ras.at.div.change <- seq(-50, 50, 2)
  
  pc.div.stk <- stack(pc.div.cur, pc.div.fut, pc.div.change)
  names(pc.div.stk) <- c("current", "future", "change")
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_alpha_div.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.div.stk, c("current", "future")), par.settings = ras.theme.div,
            margin = FALSE, colorkey = TRUE, at = ras.at.div, main = paste0(pc.main, " - alpha diversity")))
  print(lp)
  dev.off()
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_alpha_div_change.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.div.stk, c("change")), par.settings = ras.theme.div.change,
            margin = FALSE, colorkey = TRUE, at = ras.at.div.change, main = paste0(pc.main, " - alpha diversity change")))
  print(lp)
  dev.off()
#}

## 3. height map productions -----------------------------------------------------
#if(do.height.map){
  cat("\nCurent Height calculation...")
  shrub.height <- merge(data.frame(Biomod.name = shrub.list), sp.tab, all.x = TRUE, all.y = FALSE)$All.height.median
  
  pc.cur.height.stk <- pc.shrub.cur.stk * shrub.height
  pc.shrub.cur.stk[pc.shrub.cur.stk == 0] <- NA
  pc.cur.height.mean <- mean(pc.cur.height.stk, na.rm = TRUE) + land_mask
  writeRaster(pc.cur.height.mean, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_current_height_mean.grd")), overwrite = TRUE)
  pc.cur.height.max <- max(pc.cur.height.stk, na.rm = TRUE) + land_mask
  writeRaster(pc.cur.height.max, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_current_height_max.grd")), overwrite = TRUE)
  pc.cur.height.median <- calc(pc.cur.height.stk, fun = median, na.rm = TRUE) + land_mask
  writeRaster(pc.cur.height.median, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_current_height_median.grd")), overwrite = TRUE)
  
  cat("\nFuture Height calculation...")
  pc.fut.height.stk <- pc.shrub.fut.stk * shrub.height
  pc.shrub.fut.stk[pc.shrub.fut.stk == 0] <- NA
  pc.fut.height.mean <- mean(pc.fut.height.stk, na.rm = TRUE) + land_mask
  writeRaster(pc.fut.height.mean, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_futConsensus_height_mean.grd")), overwrite = TRUE)
  pc.fut.height.max <- max(pc.fut.height.stk, na.rm = TRUE) + land_mask
  writeRaster(pc.fut.height.max, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_futConsensus_height_max.grd")), overwrite = TRUE)
  pc.fut.height.median <- calc(pc.fut.height.stk, fun = median, na.rm = TRUE) + land_mask
  writeRaster(pc.fut.height.median, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_futConsensus_height_median.grd")), overwrite = TRUE)
  
  cat("\nHeight Change calculation...")
  pc.height.mean.change <- pc.fut.height.mean - pc.cur.height.mean  
  pc.height.max.change <- pc.fut.height.max - pc.cur.height.max  
  pc.height.median.change <- pc.fut.height.median - pc.cur.height.median
  
  cat("\nProduce Height maps...")
  ## define a custom color theme for the raster
  ras.theme.height <- rasterTheme(region = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
  ras.at.height <- seq(0, 3, 0.05)
  
  ras.theme.height.change <- rasterTheme(region = c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
  ras.at.height.change <- seq(-3, 3, 0.1)
  
  pc.height.mean.stk <- stack(pc.cur.height.mean, pc.fut.height.mean, pc.height.mean.change)
  names(pc.height.mean.stk) <- c("current", "future", "change")
  
  pc.height.max.stk <- stack(pc.cur.height.max, pc.fut.height.max, pc.height.max.change)
  names(pc.height.max.stk) <- c("current", "future", "change")
  
  pc.height.median.stk <- stack(pc.cur.height.median, pc.fut.height.median, pc.height.median.change)
  names(pc.height.median.stk) <- c("current", "future", "change")
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_mean_height.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.height.mean.stk, c("current", "future")), par.settings = ras.theme.height,
            margin = FALSE, colorkey = TRUE, at = ras.at.height, main = paste0(pc.main, " - mean height")))
  print(lp)
  dev.off()
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_mean_height_change.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.height.mean.stk, c("change")), par.settings = ras.theme.height.change,
            margin = FALSE, colorkey = TRUE, at = ras.at.height.change, main = paste0(pc.main, " - mean height change")))
  print(lp)
  dev.off()
  
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_max_height.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.height.max.stk, c("current", "future")), par.settings = ras.theme.height,
            margin = FALSE, colorkey = TRUE, at = ras.at.height, main = paste0(pc.main, " - max height")))
  print(lp)
  dev.off()
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_max_height_change.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.height.max.stk, c("change")), par.settings = ras.theme.height.change,
            margin = FALSE, colorkey = TRUE, at = ras.at.height.change, main = paste0(pc.main, " - max height change")))
  print(lp)
  dev.off()
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_median_height.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.height.median.stk, c("current", "future")), par.settings = ras.theme.height,
            margin = FALSE, colorkey = TRUE, at = ras.at.height, main = paste0(pc.main, " - median height")))
  print(lp)
  dev.off()
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_median_height_change.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.height.median.stk, c("change")), par.settings = ras.theme.height.change,
            margin = FALSE, colorkey = TRUE, at = ras.at.height.change, main = paste0(pc.main, " - median height change")))
  print(lp)
  dev.off()
#}

## 3. dispersal map productions -----------------------------------------------------
#if(do.dispersal.map){
  cat("\nCurent dispersal calculation...")
  tamme.tab <- read.table("~/BRISCA/briscahub/data/sp.list_red_tamme_disp.txt", h = TRUE)
  shrub.dispersal <- merge(data.frame(Biomod.name = shrub.list), tamme.tab, all.x = TRUE, all.y = FALSE)$log10MDD
  
  pc.cur.dispersal.stk <- pc.shrub.cur.stk * shrub.dispersal
  pc.cur.dispersal.stk[pc.cur.dispersal.stk == 0] <- NA
  pc.cur.dispersal.mean <- mean(pc.cur.dispersal.stk, na.rm = TRUE) + land_mask
  writeRaster(pc.cur.dispersal.mean, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_current_dispersal_mean.grd")), overwrite = TRUE)
 
  cat("\nFuture dispersal calculation...")
  pc.fut.dispersal.stk <- pc.shrub.fut.stk * shrub.dispersal
  pc.fut.dispersal.stk[pc.fut.dispersal.stk == 0] <- NA
  pc.fut.dispersal.mean <- mean(pc.fut.dispersal.stk, na.rm = TRUE) + land_mask
  writeRaster(pc.fut.dispersal.mean, filename= file.path(out.dir.ras, paste0(pc.pattern, bin.pattern, "_futConsensus_dispersal_mean.grd")), overwrite = TRUE)

  cat("\ndispersal Change calculation...")
  pc.dispersal.mean.change <- pc.fut.dispersal.mean - pc.cur.dispersal.mean  
  
  cat("\nProduce dispersal maps...")
  ## define a custom color theme for the raster
  ras.theme.dispersal <- rasterTheme(region = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
  ras.at.dispersal <- seq(0, 3, 0.05)
  
  ras.theme.dispersal.change <- rasterTheme(region = c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
  ras.at.dispersal.change <- seq(-3, 3, 0.1)
  
  pc.dispersal.mean.stk <- stack(pc.cur.dispersal.mean, pc.fut.dispersal.mean, pc.dispersal.mean.change)
  names(pc.dispersal.mean.stk) <- c("current", "future", "change")
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_mean_dispersal.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.dispersal.mean.stk, c("current", "future")), par.settings = ras.theme.dispersal,
            margin = FALSE, colorkey = TRUE, at = ras.at.dispersal, main = paste0(pc.main, " - mean dispersal")))
  print(lp)
  dev.off()
  
  png(file.path(out.dir.figs, paste0(pc.pattern, bin.pattern, "_mean_dispersal_change.png")), width = 600, height = 300, res = 80, type = 'cairo')
  lp <- (levelplot(subset(pc.dispersal.mean.stk, c("change")), par.settings = ras.theme.dispersal.change,
            margin = FALSE, colorkey = TRUE, at = ras.at.dispersal.change, main = paste0(pc.main, " - mean dispersal change")))
  print(lp)
  dev.off()
#}

t.stop <- Sys.time()
cat("\njob ended on:", strftime(t.stop, "%m/%d/%y - %H:%M:%S"))
cat("\n\t> job took:")
print(difftime(t.stop, t.start))

quit('no')
