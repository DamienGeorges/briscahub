################################################################################
##' @title produce models projections and ensemble projections
##'
##' @author Damien G. 
##' @date 10/09/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   This script will produce projections of  species distribution models with coordinates as predictors build
##'   via the script "shrub_pure_climat_modelling_xy.R".
##'   
##'   We will consider :
##'     - 189 shrubs and tree species
##'     - current conditions
##'   A piece of code to generate parameters is available at the end of this script.
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


rm(list=ls())

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
job.id <- as.numeric(args[1])

## test ===
## job.id <- 1


## definig the machine where the script will run -------------------------------
host = "idiv_cluster"

## input/output directories depending on the host ------------------------------
if(host == "pinea"){
  ## TODO (Damien)
} else if(host == "brisca_cluster"){
  ## TODO (Damien)
} else if (host == "idiv_cluster"){
  # path to the directory where models have been computed
#   in.mod <- "/work/georges/BRISCA/Biomod_pure_climate"
  in.mod <- "/work/georges/BRISCA/Biomod_pure_climate_invdist"
  # path to parameter table
  param.file <- "/work/georges/BRISCA/grid_params/params_spcp_xy.txt" ## first run (10G ram)
#    param.file <- "/work/georges/BRISCA/grid_params/params_spcp20G.txt" ## second run (20G ram)
}

## create the output directory and change the working directory ----------------
setwd(in.mod)

## get the job parameters ------------------------------------------------------
param.tab <- read.table(param.file, header = FALSE, sep = " ")
sp.name <- as.character(param.tab[job.id, 2])
path.to.expl.var <- as.character(param.tab[job.id, 3])
# distance to occ layers
in.dist <- "/data/idiv_sdiv/brisca/Data/InvDistanceRaster"
# load Brisca param files
sp.tab <- read.table("/home/georges/BRISCA/briscahub/data/sp.list_08102015_red.txt", header = TRUE, sep = "\t")
sp.name.orig <- as.character(sp.tab[is.element(sp.tab$Biomod.name, sp.name), "Genus.species"])


## require libraries -----------------------------------------------------------
require(biomod2, lib.loc='~/R/biomod2_pkg/biomod2_3.1-73-04') ## version 1.3-73-02 (= the same than 1.3.73 with a trick not to save rasters in tmp dir)

# rasterOptions(overwrite=TRUE, tmptime=24, chunksize=1e+09, maxmemory=1e+09)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
##              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive



## load models outputs and explanatory variables -------------------------------
## load models
mod.name <- load(file.path(in.mod, sp.name, paste0(sp.name, ".pure_climat.models.out")))
bm.mod <- get(mod.name)
rm(list = mod.name)

## load ensemble models
ensmod.name <- load(file.path(in.mod, sp.name, paste0(sp.name, ".pure_climatensemble.models.out")))
bm.ensmod <- get(ensmod.name)
rm(list = ensmod.name)

## load explanatory variables
## define the projection system
proj <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 

## bioclimatic variables
bio <- stack(file.path(path.to.expl.var, "bio", ifelse(grepl("Current", path.to.expl.var), "bioproj.grd", "bioproj_multi.grd")))
## degree day
ddeg <- raster(file.path(path.to.expl.var, "tave10_esri", "ddeg.grd"))
## distance to closest occurence
invdist.laea <- raster(file.path(in.dist,
                              paste0("InvDist_", sp.name, ".grd")))
## merge all cliimatic variables
expl.stk <- stack(ddeg, subset(bio, c(6, 10, 18, 19)), invdist.laea)

## do projections --------------------------------------------------------------
##define the projection name
if(grepl("Current", path.to.expl.var)){
  bm.proj.name <- "pure_climat_current"
} else{
  bm.proj.name <- paste0("pure_climat_", sub("/", "_", sub("^.*Full_arctic_30_north/", "", path.to.expl.var))) 
}

##' @note !!!! here is a horrorful trick to prevent the species 38 Chamaepericlymenum.unalaschkense
##'  to fail because of maxent models => will just be removed from the computation
selected.models <- 'all'
if(job.id == 38) selected.models <- grep("_MAXENT", get_built_models(bm.mod), value = TRUE, invert = TRUE)

## do single models projections
bm.mod.proj <- BIOMOD_Projection(modeling.output = bm.mod,
                                 new.env = expl.stk,
                                 proj.name = bm.proj.name,
                                 selected.models = selected.models,
#                                  binary.meth = c('TSS'), ## no needd to produce binary here
                                 build.clamping.mask = FALSE,
                                 compress = TRUE,
                                 do.stack = FALSE,
                                 keep.in.memory = TRUE)

##' @note  causeit require significatively more memory, I decided to do ensemble projections 
##'   in a separate step.

# ## do ensemble models projections
# bm.ensmod.proj <- BIOMOD_EnsembleForecasting(EM.output = bm.ensmod,
#                                              projection.output = bm.mod.proj,
#                                              binary.meth = c('TSS'),
#                                              compress = TRUE)


quit('no')

## end of script ---------------------------------------------------------------

#  ## create the parameter files for the grid -------------------------------------
#  
#  ## on idiv_cluster
#  out.dir <- "/work/georges/BRISCA/grid_params/"
#  dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
#  
#  ## get all species names reduced list of species
#  sp.tab <- read.table("/home/georges/BRISCA/briscahub/data/sp.list_08102015_red.txt", header = TRUE, sep = "\t")
#  sp.list <- as.character(sp.tab[, "Biomod.name"])
#  
#  ## ## or get all species names from computed models
#  ## in.spp <- "/work/georges/BRISCA/Biomod_pure_climate_xy"
#  ## sp.list <- list.files(in.spp)
#  
#  path.to.cur.expl.var <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Current/Processed/Projected"
#  path.to.expl.var <- c(path.to.cur.expl.var)
#  
#  params <- expand.grid(sp.list = sp.list,
#                        path.to.expl.var = path.to.expl.var)
#  write.table(params, file = file.path(out.dir, "params_spcp_xy.txt"), sep = " ", 
#  		quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)
