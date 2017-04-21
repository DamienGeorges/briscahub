################################################################################
##' @title produce models projections and ensemble projections
##'
##' @author Damien G. 
##' @date 10/09/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
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
##'   
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
## job.id <- 4


## definig the machine where the script will run -------------------------------
host = "idiv_cluster"
type = "incl_tree"

## require libraries -----------------------------------------------------------
.libPaths("~/R/x86_64-pc-linux-gnu-library/3.2")
require(biomod2, lib.loc='~/R/biomod2_pkg/biomod2_3.1-73-04') ## version 1.3-73-02 (= the same than 1.3.73 with a trick not to save rasters in tmp dir)
require(rgdal)
require(data.table)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE)

## input/output directories depending on the host ------------------------------
if(host == "pinea"){
  ## TODO (Damien)
} else if(host == "brisca_cluster"){
  ## TODO (Damien)
} else if (host == "idiv_cluster"){
  # path to the directory where models have been computed
  in.mod <- paste0("/work/georges/BRISCA/Biomod_climate_and_biointer_", type, "_2017-04-07")
  # path to parameter table
  # param.file <- paste0("/work/georges/BRISCA/grid_params/params_scabp_", type, ".txt") ## first run (10G ram)
  param.file <- paste0("/work/georges/BRISCA/grid_params/params_scabp_", type, "_missing.txt") ## first run (10G ram)
  ras.ref.file <- "/data/idiv_sdiv/brisca/results/raster_ref_27_02_2017.grd"
}

## create the output directory and change the working directory ----------------
setwd(in.mod)

## get the job parameters ------------------------------------------------------
# param.tab <- read.table(param.file, header = FALSE, sep = "\t")
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

##define the projection name
biointer.str <- sub(".grd", "", sub("^.*_bio_inter_", "", path.to.biointer.stk))
if(grepl("Current", path.to.clim.var)){
  bm.proj.name <- paste0("pure_climat_current", biointer.str)
} else{
  bm.proj.name <- paste0("pure_climat_", sub("/", "_", sub("^.*Full_arctic_30_north/", "", path.to.clim.var)), biointer.str) 
}

## load explanatory variables
## define the projection system
proj <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## bioclimatic variables
bio <- stack(file.path(path.to.clim.var, "bio", ifelse(grepl("Current", path.to.clim.var), "bioproj.grd", "bioproj_multi.grd")))
## degree day
ddeg <- raster(file.path(path.to.clim.var, "tave10_esri", "ddeg.grd"), crs = proj)
## biotic interaction
biointer <- stack(path.to.biointer.stk)
## select the oppropriate layer
biointer <- subset(biointer, sub(biointer.str, "", sub("pure_climat_", "", bm.proj.name)))
## added for the testing session of biointeraction maps
biointer <- projectRaster(biointer, bio)
names(biointer) <- "biointer"


## merge all cliimatic variables
expl.stk <- stack(ddeg, subset(bio, c(6, 10, 18, 19)), biointer)
expl.stk.names <- names(expl.stk)

## reproject the explanatory variables to work in the right projection system
## load teh ref mask
ras.ref <- raster(ras.ref.file)
expl.stk <- projectRaster(expl.stk, ras.ref)

## ensure that exactly all the same cells are define in explanatory rasters
## function to define the intersect of rasters
intersect_mask <- function(x){
  values_x <- getValues(x)
  inter_x <- values_x %*% rep(1,nlayers(x))
  mask <- setValues(subset(x,1),values = (inter_x>0))
  return(mask)
}

## keep only all cells that are defined for all layers
expl.stk <- stack(mask(expl.stk, intersect_mask(expl.stk)))
names(expl.stk) <- expl.stk.names

## do projections --------------------------------------------------------------

## do single models projections
bm.mod.proj <- BIOMOD_Projection(modeling.output = bm.mod,
                                 new.env = expl.stk,
                                 proj.name = bm.proj.name,
                                 build.clamping.mask = FALSE,
                                 compress = TRUE,
                                 do.stack = FALSE)

##' @note  causeit require significatively more memory, I decided to do ensemble projections 
##'   in a separate step.

quit('no')

## end of script ---------------------------------------------------------------

## create the parameter files for the grid -------------------------------------

# ## on idiv_cluster
# library(dplyr)
# type <- "incl_tree"
# out.dir <- "/work/georges/BRISCA/grid_params/"
# dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
# sp.list <- read.table("~/BRISCA/briscahub/data/sp.list_03.03.2017.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# sp.list <- sp.list$Biomod.name
# 
# ## define the gcm and rcp we want to consider
# rcp.list <- c("RCP_2.6_2080", "RCP_4.5_2080", "RCP_6.0_2080", "RCP_8.5_2080")
# gcm.list <- c("cesm1_cam5", "gfdl_esm2m", "miroc_miroc5", "mri_cgcm3", "ncar_ccsm4",
#               "nimr_hadgem2ao", "csiro_mk360")
# rcp.gcm.comb <- expand.grid(rcp.list = rcp.list,
#                             gcm.list = gcm.list)
# from.path.to.fut.expl.var <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Future/CIAT_AR5_bio_prec_tmean_tmax_tmin/Processed/Projected_polar_laea_10km/Full_arctic_30_north"
# path.to.fut.expl.var <- file.path(from.path.to.fut.expl.var, rcp.gcm.comb$rcp.list, rcp.gcm.comb$gcm.list)
# path.to.cur.expl.var <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Current/Processed/Projected"
# path.to.clim.var <- c(path.to.cur.expl.var, path.to.fut.expl.var)
# 
# params <- expand.grid(sp.list = sp.list,
#                       path.to.clim.var = path.to.clim.var)
# 
# params.no.disp.biointer <- params.ulim.disp.biointer <- params.max.disp.biointer <- params
# 
# params.no.disp.biointer$path.to.biointer.stk <- paste0("/work/georges/BRISCA/Biomod_biotic_interaction_maps_", type, "_2017-04-06/", params.no.disp.biointer$sp.list, "_bio_inter_no_dipersal.grd")
# params.max.disp.biointer$path.to.biointer.stk <- paste0("/work/georges/BRISCA/Biomod_biotic_interaction_maps_", type, "_2017-04-06/", params.no.disp.biointer$sp.list, "_bio_inter_max_dipersal.grd")
# params.ulim.disp.biointer$path.to.biointer.stk <- paste0("/work/georges/BRISCA/Biomod_biotic_interaction_maps_", type, "_2017-04-06/", params.no.disp.biointer$sp.list, "_bio_inter_unlimited_dipersal.grd")
# 
# ## remove all the non no dispersal based biotic interaction for current condition
# params.max.disp.biointer <- params.max.disp.biointer[!grepl('current', params.max.disp.biointer$path.to.clim.var), ]
# params.ulim.disp.biointer <- params.ulim.disp.biointer[!grepl('current', params.ulim.disp.biointer$path.to.clim.var), ]
# 
# 
# params <- rbind(params.no.disp.biointer, params.max.disp.biointer, params.ulim.disp.biointer)
# 
# ## subselect a part of params?
# # params <- params[!grepl("_min_disp", params$path.to.biointer.stk), ]
# 
# ## reorder the table by species names
# params <- params %>% arrange(sp.list, path.to.clim.var, path.to.biointer.stk)
# rownames(params) <- NULL
# 
# write.table(params, file = file.path(out.dir, paste0("params_scabp_", type,".txt")), sep = "\t",
#             quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)


