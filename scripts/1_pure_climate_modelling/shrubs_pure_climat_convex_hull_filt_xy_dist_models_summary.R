################################################################################
##' @title produce summary graphs to try to disantangle what is the best way to 
##'   filter our models projections
##'
##' @author Damien G. 
##' @date 12/10/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build some visual tools to help us to get an
##'   idea of if it make more sens to filter our projection via convex hull or
##'   models having been built with coordiantes as predictors
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
# args <- commandArgs(trailingOnly = TRUE)
# sp.id <- as.numeric(args[1])

  
## -- load needed packages ----------------------------------------------------- 
library(biomod2, lib.loc = "~/R/biomod2_pkg/biomod2_3.1-73-04")
library(raster)
library(grid)
library(gridExtra)
library(rasterVis)

## -- define path to models and to output directories --------------------------
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final_summary_v2"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

## -- define how we will split our final graph ---------------------------------
panel.ncol <- 3
panel.nrow <- 20
img.width <- panel.ncol * 300
img.height <- panel.nrow * 300

for(sp.id in 1:189){
  cat("\n> sp:", sp.id, "/189 ---------------------------------------------\n")
  
  ## -- create a new png file each 20 species ----------------------------------
  is.new.graph <- ((sp.id - 1) %% (panel.nrow)) == 0
  if(is.new.graph){
    graph.id <- (sp.id %/% (panel.nrow)) + 1
    ## open a png device
    file.name <- file.path(out.dir, paste0("spcm_convex_hull_full_filt_invdist_models_test", graph.id, ".png"))
    cat("\n> Creating", file.name)
    png(file.name, 
        width = img.width, height = img.height, res = 80, type = 'cairo')
  }
  
  ## get the pannel position of the species
  sp.panel.pos <- (sp.id) %% (panel.nrow)
  if(sp.panel.pos == 0) sp.panel.pos <- (panel.nrow)
  
  ## -- select a species e want to work with -------------------------------------
  sp.name <- sp.tab$Genus.species[sp.id]
  sp.name.bm <- sp.tab$Biomod.name[sp.id]
  
  ## -- load convex hull data ----------------------------------------------------
  convex.hull.dir <- "~/BRISCA/workdir/brsica_shrubs_convex_hull_full_filtered"
  assign(paste0("convex.hull.poly.area.group", sp.panel.pos), get(load(file.path(convex.hull.dir, paste0(sp.name, "_convex_hull_poly_area_group.RData")))))
  
  ## -- load species occurences --------------------------------------------------
  in.spp <- "/home/georges/BRISCA/workdir/Occurrence.tables.combined.all.sources" 
  pres.pts.file <- list.files(in.spp, pattern = paste0(sp.name, ".txt"), full.names = TRUE)
  pres.pts <- read.table(pres.pts.file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  pres.pts <- na.omit(pres.pts)
  coordinates(pres.pts) <- pres.pts[, c('X', 'Y')]
  assign(paste0("pres.pts", sp.panel.pos), pres.pts)
  
  ## -- load models with coordinates as predictor projections --------------------
#   proj.xy.bin <- raster(file.path("/work/georges/BRISCA/Biomod_pure_climate_xy", 
#                                   sp.name.bm, "proj_pure_climat_current",
#                                   "individual_projections",
#                                   paste0(sp.name.bm, "_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd")))
  
  ## -- load models with distance to occurance as predictor projections ----------
  proj.inv.dist.bin <- try(raster(file.path("/work/georges/BRISCA/Biomod_pure_climate_invdist", 
                                  sp.name.bm, "proj_pure_climat_current",
                                  "individual_projections",
                                  paste0(sp.name.bm, "_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"))))
  ##' @note curently the species 38 distance model has failed so we will make a
  ##'   trick not to block the computation 
  if(inherits(proj.inv.dist.bin, "try-error")){proj.inv.dist.bin <- raster("/data/idiv_sdiv/brisca/Data/no_interaction_mask.grd"); proj.dist.bin[] <- NA}
  
  ## -- load current projections -------------------------------------------------
  proj.cur <- raster(file.path(mod.dir, sp.name.bm, "proj_pure_climat_current",
                               "individual_projections", 
                               paste0(sp.name.bm, "_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd")))
  
  ## -- produce the plot ---------------------------------------------------------
  
  ## intitialise the list of argument if needed
  if(sp.panel.pos == 1){
    lis.args <- list()
  }
  ## define a custom color theme for the raster
  ras.theme <- rasterTheme(region=brewer.pal(9, 'Greens'))
  ## define a custom colorkey to represent our proj on 0-1 instead of 0-1000 scale
  ras.at <- seq(0, 1000, 50)
  ras.labels <- ras.at/1000
  ras.labels[ras.at %% 250 != 0] <- ""
  ras.colorkey <- list(at = ras.at, labels = ras.labels)
  
  ## first graph is the current proj + points
  pp <- levelplot(proj.cur, par.settings = ras.theme,
                  margin = FALSE, colorkey = FALSE) + 
    eval(parse(text = paste0("layer(sp.points(pres.pts", sp.panel.pos, ", col = 'yellow', alpha = .01 ))")))
  pp$ylab <- sp.name
  if(sp.panel.pos == 1) pp$main <- "current proj\n+ pts"
  ## add the graph in the list
  lis.args[[(sp.panel.pos-1) * panel.ncol + 1]] <- pp

  ## second graph is for convex hull over big area coupled with automatic clustering
  pp <- levelplot(proj.cur, par.settings = ras.theme,
                  margin = FALSE, colorkey = FALSE) + 
    eval(parse(text = paste0("layer(sp.points(pres.pts", sp.panel.pos, ", col = 'yellow', alpha = .01 ))"))) +
    eval(parse(text = paste("layer(sp.lines(convex.hull.poly.area.group", sp.panel.pos, "[[", 
                            1:length(get(paste0("convex.hull.poly.area.group", sp.panel.pos))),
                            "]], col = 'black'))", sep = "", collapse = " + ")))
  if(sp.panel.pos == 1) pp$main <- "current proj\n+ CH area-clust filt"
  ## add the graph in the list
  lis.args[[(sp.panel.pos-1) * panel.ncol + 2]] <- pp
  
  ## third graph is for the XY models
#   pp <- levelplot(proj.xy.bin, par.settings = ras.theme,
#                   margin = FALSE, colorkey = FALSE) + 
#     eval(parse(text = paste0("layer(sp.points(pres.pts", sp.panel.pos, ", col = 'yellow', alpha = .01 ))")))
#   if(sp.panel.pos == 1) pp$main <- "current proj XY mod\n+ pts"
#   ## add the graph in the list
#   lis.args[[(sp.panel.pos-1) * panel.ncol + 3]] <- pp
  
  ## fourth graph is for the dist models
  pp <- levelplot(proj.inv.dist.bin, par.settings = ras.theme,
                  margin = FALSE, colorkey = FALSE) + 
    eval(parse(text = paste0("layer(sp.points(pres.pts", sp.panel.pos, ", col = 'yellow', alpha = .01 ))")))
  if(sp.panel.pos == 1) pp$main <- "current proj dist mod\n+ pts"
  ## add the graph in the list
  lis.args[[(sp.panel.pos-1) * panel.ncol + 3]] <- pp
   
  is.last.panel <- (sp.panel.pos == (panel.nrow) | sp.id == 189)
  if(is.last.panel){
    ## debug
#     save.image("/work/georges/BRISCA/workdir/spcchfxdms.RData")
    ## end ddebug
    lis.args <- c( lis.args, ncol = panel.ncol, main = list(textGrob(paste0("\nConvex Hull vs invdist modelling filtering test",
                                                                            "\nbased on pure climate and models committee averaging"), 
                                                                     gp=gpar(fontsize=20,font=3))))
    cat("\n> Producing the graph...")
    do.call(grid.arrange, lis.args)
    dev.off()
    cat("\tdone!")
  }

} ## end loop over species for serial version
# q('no')
## -- end of script ------------------------------------------------------------
