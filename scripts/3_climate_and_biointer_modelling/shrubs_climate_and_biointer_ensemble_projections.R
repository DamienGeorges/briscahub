################################################################################
##' @title produce models  ensemble projections
##'
##' @author Damien G. 
##' @date 29/09/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   Ideally the ensemble projection procedure should be done at the same time than single models projections but this procedures were splitted in 2 parts due to memory consumtion issues (fixed now!)
##'   This script will produce projections of  species distribution models build
##'   via the script "shrub_pure_climat_modelling.R".
##'   
##'   We will consider :
##'     - ~ 200 shrubs and tree species
##'     - current conditions
##'     - future conditions forecasted in 2080 in the 5th ipcc/giec repport for 
##'       4 RCP x 6 GCM
##'       
##'   This script is designed to run projections for a single species and a single
##'   climatique condition given as input to enhance parallel processing of the 
##'   projections.
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

t.start <- Sys.time()
cat("\njob started on:", strftime(t.start, "%m/%d/%y - %H:%M:%S"))

library(biomod2, lib.loc = "~/R/biomod2_pkg/biomod2_3.1-73-07")
require(rgdal)
require(data.table)
## define here raster options to limit IO impact of jobs
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+08, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive

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
  in.mod <- "/work/georges/BRISCA/Biomod_climate_and_biointer" ## pure climate + invdist models 
  # path to parameter table
  param.file <- "/work/georges/BRISCA/grid_params/params_scabeph.txt"
}




## we will take an example of a model already computed
setwd(in.mod)

## get the job parameters ------------------------------------------------------
param.tab <- fread(param.file, header = FALSE, sep = "\t", nrows = 1, 
                   skip = job.id - 1,
                   colClasses=c("integer", "character", "character", "character"))
sp.name <- as.character(param.tab[[2]])
path.to.clim.var <- as.character(param.tab[[3]])
path.to.biointer.stk <- as.character(param.tab[[4]])

## load models outputs and explanatory variables -------------------------------
## load models
mod.name <- load(file.path(in.mod, sp.name, paste0(sp.name, ".climat_and_biointer.models.out")))
bm.mod <- get(mod.name)
rm(list = mod.name)

## load ensemble models
ensmod.name <- load(file.path(in.mod, sp.name, paste0(sp.name, ".climat_and_biointerensemble.models.out")))
bm.ensmod <- get(ensmod.name)
rm(list = ensmod.name)

## do projections --------------------------------------------------------------
##define the projection name
biointer.str <- sub(".grd", "", sub("^.*_bio_inter_filt", "", path.to.biointer.stk))

if(grepl("Current", path.to.clim.var)){
  bm.proj.name <- paste0("pure_climat_current", biointer.str)
} else{
  bm.proj.name <- paste0("pure_climat_", sub("/", "_", sub("^.*Full_arctic_30_north/", "", path.to.clim.var)), biointer.str) 
}

bm.mod.proj.name <- load(file.path(in.mod, 
                                   sp.name, 
                                   paste0("proj_", bm.proj.name),
                                   paste0(sp.name, ".", bm.proj.name, ".projection.out")))
bm.mod.proj <- get(bm.mod.proj.name)
rm(list = bm.mod.proj.name)

## do ensemble models projections
bm.ensmod.proj <- BIOMOD_EnsembleForecasting(EM.output = bm.ensmod,
						#selected.models = grep('EMca', get_built_models(bm.ensmod), value = TRUE),
                                             projection.output = bm.mod.proj,
                                             binary.meth = c('TSS'),
                                             do.stack = FALSE,
                                             compress = TRUE)
t.stop <- Sys.time()
cat("\njob ended on:", strftime(t.stop, "%m/%d/%y - %H:%M:%S"))
cat("\n\t> job took:")
print(difftime(t.stop, t.start))

quit('no')

