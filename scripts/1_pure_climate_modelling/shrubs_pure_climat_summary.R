################################################################################
##' @title produce pure cllimate modelling summary graphs
##'
##' @author Damien G. 
##' @date 12/10/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build some visual tools to help us to get an
##'   idea of how accurate our models are.
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

for(sp.id in 1:189){
  cat("\n> sp:", sp.id, "/189 ---------------------------------------------\n")
  
## -- load needed packages ----------------------------------------------------- 
library(biomod2, lib.loc = "~/R/biomod2_pkg/biomod2_3.1-73-04")
library(ggplot2)
library(raster)
library(grid)
library(gridExtra)
library(rasterVis)

## -- define path to models and to output directories --------------------------
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final_summary"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
briscahub.dir <- "/home/georges/BRISCA/briscahub"

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

## -- select a species e want to work with -------------------------------------
sp.name <- sp.tab$Genus.species[sp.id]
sp.name.bm <- sp.tab$Biomod.name[sp.id]

## -- load formal data ---------------------------------------------------------
in.spp <- "/data/idiv_sdiv/brisca/SDM_sessions/Presence-PseudoAbsence_thinned/Data_output/gbif_biosc_hult_usgs_thined_10000" 
pres.thin.file <- list.files(in.spp, 
                             pattern = paste0("^pres_and_10000_PA_thin_", 
                                              gsub(" ", "_", sp.name), ".csv"),
                             full.names = TRUE)
if(!length(pres.thin.file)){
  in.spp <- "/data/idiv_sdiv/brisca/SDM_sessions/Presence-PseudoAbsence_thinned/Data_output/gbif_biosc_hult_thined_10000" 
  pres.thin.file <- list.files(in.spp, 
                               pattern = paste0("^pres_and_10000_PA_thin_", 
                                                gsub(" ", "_", sp.name), ".csv"),
                               full.names = TRUE)
}

## load the .csv
pres.thin <- read.csv(pres.thin.file)

## add a column defining if a point should be consider as a presence or an absence
pres.thin[is.na(pres.thin)] <- 0
pres.thin$sum.samp <- rowSums(pres.thin[, 4:13]) ## count the number of time a cell have been considered as presence
pres.thin$status <- pres.thin$sum.samp > 0 ## define a state according to each cell (0: absence, 1: presence)
pres.thin.sp <- SpatialPoints(pres.thin[pres.thin$status == 1, c("x", "y")], 
                              proj4string=CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

## -- define how we will split our final graph ---------------------------------
panel.ncol <- 4
panel.nrow <- 7

## -- load current projections -------------------------------------------------
proj.cur <- raster(file.path(mod.dir, sp.name.bm, "proj_pure_climat_current",
                             "individual_projections", 
                             paste0(sp.name.bm, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.grd")))

proj.cur.bin <- raster(file.path(mod.dir, sp.name.bm, "proj_pure_climat_current",
                                 "individual_projections", 
                                 paste0(sp.name.bm, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd")))

## -- load future projections ---------------------------------------------------
## define the gcm and rcp we want to consider
rcp.list <- c("RCP_2.6_2080", "RCP_4.5_2080", "RCP_6.0_2080", "RCP_8.5_2080")
gcm.list <- c("cesm1_cam5", "gfdl_esm2m", "miroc_miroc5", "mri_cgcm3", "ncar_ccsm4",
              "nimr_hadgem2ao")
rcp.gcm.comb <- expand.grid(rcp.list = rcp.list,
                            gcm.list = gcm.list)
## add columns to define the place of the projection in our final layout
rcp.gcm.comb$rcp.id <- as.numeric(as.factor(rcp.gcm.comb$rcp.list))
rcp.gcm.comb$gcm.id <- as.numeric(as.factor(rcp.gcm.comb$gcm.list))
rcp.gcm.comb$panel.pos <- rcp.gcm.comb$gcm.id * panel.ncol + rcp.gcm.comb$rcp.id 

## get the name of the projection file asssociated to the rcp/gcm comb
rcp.gcm.comb$file <- file.path(mod.dir, sp.name.bm, 
                               paste0("proj_pure_climat_", rcp.gcm.comb$rcp.list, "_", rcp.gcm.comb$gcm.list),
                               "individual_projections", 
                               paste0(sp.name.bm, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.grd"))
## load all the projections within a stack
rcp.gcm.stk <- stack(rcp.gcm.comb$file)

## -- get the models score graphs ----------------------------------------------
## load single models
bm.mod.name <- load(file.path(mod.dir, sp.name.bm, 
                              paste0(sp.name.bm, ".pure_climat.models.out")))
bm.mod <- get(bm.mod.name)
rm(list = bm.mod.name)
## load ensemble models
bm.ensmod.name <- load(file.path(mod.dir, sp.name.bm, 
                              paste0(sp.name.bm, ".pure_climatensemble.models.out")))
bm.ensmod <- get(bm.ensmod.name)
rm(list = bm.ensmod.name)
## get ensemble models scores
bm.ensmod.scores <- get_evaluations(bm.ensmod, as.data.frame = TRUE)
## remove the models without scores (cv) and keep only needed info
bm.ensmod.scores <-  bm.ensmod.scores[!is.na(bm.ensmod.scores$Testing.data), 
                                      c("Model.name", "Eval.metric", "Testing.data")]
## rename models in a friendly way
bm.ensmod.scores$Model.name <- sub("ByTSS_.*$", "", bm.ensmod.scores$Model.name)
bm.ensmod.scores.casted <- cast(bm.ensmod.scores, Model.name ~ Eval.metric, value  = "Testing.data" )
colnames(bm.ensmod.scores.casted) <- c("name",  "mean2",  "mean1")

## build the model score graph by models
mod.score.graph.by.mod <-  models_scores_graph( bm.mod,
                                         by = 'models',
                                         metrics = c('ROC','TSS'),
                                         plot = FALSE)
mod.score.graph.by.mod <- mod.score.graph.by.mod + geom_point(data = bm.ensmod.scores.casted)

## build the model score graph by data_set
mod.score.graph.by.data_set <-  models_scores_graph( bm.mod,
                                                by = 'data_set',
                                                metrics = c('ROC','TSS'),
                                                plot = FALSE)
mod.score.graph.by.data_set <- mod.score.graph.by.data_set + geom_point(data = bm.ensmod.scores.casted)


## -- produce the plot ---------------------------------------------------------
img.width <- panel.ncol * 300
img.height <- panel.nrow * 300

## open a pdf device
png(file.path(out.dir, paste0("spcm_", sp.name.bm,".png")), width = img.width, height = img.height, res = 80)

lis.args <- list()
## define a custom color theme for the raster
ras.theme <- rasterTheme(region=brewer.pal(9, 'Greens'))
## define a custom colorkey to represent our proj on 0-1 instead of 0-1000 scale
ras.at <- seq(0, 1000, 50)
ras.labels <- ras.at/1000
ras.labels[ras.at %% 250 != 0] <- ""
ras.colorkey <- list(at = ras.at, labels = ras.labels)
for(i in 1:(panel.ncol*panel.nrow)){
  ## first graph: current binary projections
  if(i == 1){
    pp <- levelplot(proj.cur.bin, par.settings = ras.theme, at = seq(0, 1, .5),
                    margin = FALSE, colorkey = FALSE) +
      layer(sp.points(coordinates(pres.thin.sp)[1:10, ], col = 'red'))
    pp$main <- "curent (bin)"  
  }

  ## first graph: current binary projections
  if(i == 2){
    pp <- levelplot(proj.cur, par.settings = ras.theme,
                    margin = FALSE, colorkey = ras.colorkey)
    pp$main <- "curent"  
  }
  
  ## graph 3 and 4 are evaluaion linked graphs
  if(i == 3){
    ## TODO(damien)
    pp <- mod.score.graph.by.mod
  }
  if(i == 4){
    ## TODO(damien)
    pp <- mod.score.graph.by.data_set
  }
  
  ## graphs 5 to 30 are future projections
  if(i >= 5){
    pp <- levelplot(subset(rcp.gcm.stk, i-4), par.settings = ras.theme, at = seq(0, 1000, 50),
                    margin = FALSE, colorkey = FALSE)
    ## add colnames 
    if(is.element(i, 5:8)){pp$main = as.character(rcp.gcm.comb$rcp.list[rcp.gcm.comb$panel.pos == i])}
    ## add rownames
    if(is.element(i, seq(5, 25, 4))){pp$ylab = as.character(rcp.gcm.comb$gcm.list[rcp.gcm.comb$panel.pos == i])}
  }
  
  ## add the graph in the list
  lis.args[[i]] <- pp
}
lis.args <- c( lis.args, ncol = panel.ncol, main = list(textGrob(paste0("\n",sp.name), gp=gpar(fontsize=20,font=3))))
do.call(grid.arrange, lis.args)
dev.off()

}
# q('no')
## -- end of script ------------------------------------------------------------
