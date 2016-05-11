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

## -- load needed packages ----------------------------------------------------- 
#library(biomod2, lib.loc = "~/R/biomod2_pkg/biomod2_3.1-73-04")
library(raster)
#library(grid)
#library(gridExtra)
library(rasterVis)

## -- define path to models and to output directories --------------------------
pc.mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
pcf.mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_filtered"
cab.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer"
cabf.mod.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_filtered"

out.dir.figs <- "/work/georges/BRISCA/Biomod_framework_summary/figs"
out.dir.ras <- "/work/georges/BRISCA/Biomod_framework_summary/summary_raster"
dir.create(out.dir.figs, recursive = TRUE, showWarnings = FALSE)
dir.create(out.dir.ras, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

shrub.list <- sp.tab$Biomod.name[ sp.tab$Growth.form.height == 'SHRUB' ]

land_mask <- raster("/data/idiv_sdiv/brisca/Data/no_interaction_mask.grd")
fut_mask <- subset(raster("/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Future/CIAT_AR5_bio_prec_tmean_tmax_tmin/Processed/Projected_polar_laea_10km/Full_arctic_30_north/RCP_2.6_2080/cesm1_cam5/bio/bioproj_multi.grd"), 1) > 0
land_mask <- land_mask * fut_mask

## 1. create a diversity map and a diversity map change ------------------------

pc.shrub.cur.ras.files <- file.path(pc.mod.dir, shrub.list, "proj_pure_climat_current", 
  "individual_projections", paste0(shrub.list, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"))
pc.shrub.cur.stk <- stack(pc.shrub.cur.ras.files)
pc.div.cur <- sum(pc.shrub.cur.stk, na.rm = TRUE)
pc.div.cur <- pc.div.cur + land_mask
writeRaster(pc.div.cur, filename= file.path(out.dir.ras, "pc_current_alpha_div.grd"),  overwrite = TRUE)

pc.shrub.fut.ras.files <- lapply(shrub.list, function(sl){
  fut.proj.dir <- dir(file.path(pc.mod.dir, sl), 
                      pattern = "proj_pure_climat_RCP", full.names = TRUE)
  return(file.path(fut.proj.dir, 
                   "individual_projections", 
                   paste0(sl, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd")))
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
library(foreach)
library(doParallel)
cl <- makeCluster(10)
registerDoParallel(cl)
pc.shrub.fut.stk.lst <- vector("list", length(shrub.list))
foreach(k = 1:length(pc.shrub.fut.ras.files), .packages = c('raster')) %dopar% {
  rf <- pc.shrub.fut.ras.files[[k]]
  pc.ca.mean <- mean(stack(rf))
  writeRaster(pc.ca.mean, filename = file.path(out.dir.ras, sub("_.*", "pure_cllimate_mean_of_all_fut_CAbin.grd", basename(rf[1]))), overwrite = TRUE)
  pc.ca.maj <- pc.ca.mean >= 0.5
  pc.shrub.fut.stk.lst[[k]] <- pc.ca.maj
}


pc.shrub.fut.stk <- stack(pc.shrub.fut.stk.lst)
# ## or reload from hard drive
# pc.shrub.fut.stk <- stack(file.path(out.dir.ras, paste0(shrub.list, "pure_cllimate_mean_of_all_fut_CAbin.grd"))) >= 0.5
pc.div.fut <- sum(pc.shrub.fut.stk, na.rm = TRUE)
pc.div.fut <- pc.div.fut + land_mask
writeRaster(pc.div.fut, filename= file.path(out.dir.ras, "pc_futConcensus_alpha_div.grd"), overwrite = TRUE)

pc.div.change <- pc.div.fut - pc.div.cur


## define a custom color theme for the raster
ras.theme.div <- rasterTheme(region = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
ras.at.div <- seq(0, 100, 2)

ras.theme.div.change <- rasterTheme(region = c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
ras.at.div.change <- seq(-50, 50, 2)

pc.div.stk <- stack(pc.div.cur, pc.div.fut, pc.div.change)
names(pc.div.stk) <- c("current", "future", "change")

png(file.path(out.dir.figs, "pc_alpha_div.png"), width = 600, height = 300, res = 80, type = 'cairo')
levelplot(subset(pc.div.stk, c("current", "future")), par.settings = ras.theme.div,
          margin = FALSE, colorkey = TRUE, at = ras.at.div, main = "Pure Climate - alpha diversity")
dev.off()

png(file.path(out.dir.figs, "pc_alpha_div_change.png"), width = 600, height = 300, res = 80, type = 'cairo')
levelplot(subset(pc.div.stk, c("change")), par.settings = ras.theme.div.change,
          margin = FALSE, colorkey = TRUE, at = ras.at.div.change, main = "Pure Climate - alpha diversity change")
dev.off()

## height map productions -----------------------------------------------------
shrub.height <- merge(data.frame(Biomod.name = shrub.list), sp.tab, all.x = TRUE, all.y = FALSE)$All.height.median

pc.cur.height.stk <- pc.shrub.cur.stk * shrub.height
pc.cur.height.mean <- mean(pc.cur.height.stk, na.rm = TRUE) + land_mask
writeRaster(pc.cur.height.mean, filename= file.path(out.dir.ras, "pc_current_height_mean.grd"), overwrite = TRUE)
pc.cur.height.max <- max(pc.cur.height.stk, na.rm = TRUE) + land_mask
writeRaster(pc.cur.height.max, filename= file.path(out.dir.ras, "pc_current_height_max.grd"), overwrite = TRUE)

pc.fut.height.stk <- pc.shrub.fut.stk * shrub.height
pc.fut.height.mean <- mean(pc.fut.height.stk, na.rm = TRUE) + land_mask
writeRaster(pc.fut.height.mean, filename= file.path(out.dir.ras, "pc_futConsensus_height_mean.grd"), overwrite = TRUE)
pc.fut.height.max <- max(pc.fut.height.stk, na.rm = TRUE) + land_mask
writeRaster(pc.fut.height.max, filename= file.path(out.dir.ras, "pc_futConsensus_height_max.grd"), overwrite = TRUE)

pc.height.mean.change <- pc.fut.height.mean - pc.cur.height.mean  
pc.height.max.change <- pc.fut.height.max - pc.cur.height.max  

## define a custom color theme for the raster
ras.theme.height <- rasterTheme(region = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
ras.at.height <- seq(0, 3, 0.05)

ras.theme.height.change <- rasterTheme(region = c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac'))
ras.at.height.change <- seq(-3, 3, 0.1)

pc.height.mean.stk <- stack(pc.cur.height.mean, pc.fut.height.mean, pc.height.mean.change)
names(pc.height.mean.stk) <- c("current", "future", "change")

pc.height.max.stk <- stack(pc.cur.height.max, pc.fut.height.max, pc.height.max.change)
names(pc.height.max.stk) <- c("current", "future", "change")


png(file.path(out.dir.figs, "pc_mean_height.png"), width = 600, height = 300, res = 80, type = 'cairo')
levelplot(subset(pc.height.mean.stk, c("current", "future")), par.settings = ras.theme.height,
          margin = FALSE, colorkey = TRUE, at = ras.at.height, main = "Pure Climate - mean height")
dev.off()

png(file.path(out.dir.figs, "pc_mean_height_change.png"), width = 600, height = 300, res = 80, type = 'cairo')
levelplot(subset(pc.height.mean.stk, c("change")), par.settings = ras.theme.height.change,
          margin = FALSE, colorkey = TRUE, at = ras.at.height.change, main = "Pure Climate - mean height change")
dev.off()


png(file.path(out.dir.figs, "pc_max_height.png"), width = 600, height = 300, res = 80, type = 'cairo')
levelplot(subset(pc.height.max.stk, c("current", "future")), par.settings = ras.theme.height,
          margin = FALSE, colorkey = TRUE, at = ras.at.height, main = "Pure Climate - max height")
dev.off()

png(file.path(out.dir.figs, "pc_max_height_change.png"), width = 600, height = 300, res = 80, type = 'cairo')
levelplot(subset(pc.height.max.stk, c("change")), par.settings = ras.theme.height.change,
          margin = FALSE, colorkey = TRUE, at = ras.at.height.change, main = "Pure Climate - max height change")
dev.off()


