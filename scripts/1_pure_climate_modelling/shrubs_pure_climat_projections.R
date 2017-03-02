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
##'  - 06/01/2016: launch the projection procedure for the corrected version of the 22 shru
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
  # in.mod <- "/work/georges/BRISCA/Biomod_pure_climate_usgs_no_flaws"
  in.mod <- "/work/georges/BRISCA/Biomod_pure_climate_strange_distrib"
  # path to parameter table
#   param.file <- "/work/georges/BRISCA/grid_params/params_spcp.txt" ## first run (10G ram)
#    param.file <- "/work/georges/BRISCA/grid_params/params_spcp20G.txt" ## second run (20G ram)
  param.file <- "/work/georges/BRISCA/grid_params/params_spcp.txt" ## first run (10G ram)
  ras.ref.file <- "/data/idiv_sdiv/brisca/results/raster_ref_27_02_2017.grd"
}

## create the output directory and change the working directory ----------------
setwd(in.mod)

## get the job parameters ------------------------------------------------------
param.tab <- read.table(param.file, header = FALSE, sep = " ")
sp.name <- as.character(param.tab[job.id, 2])
path.to.expl.var <- as.character(param.tab[job.id, 3])



## require libraries -----------------------------------------------------------
.libPaths("~/R/x86_64-pc-linux-gnu-library/3.2/")
require(biomod2, lib.loc='~/R/biomod2_pkg/biomod2_3.1-73-04') ## version 1.3-73-02 (= the same than 1.3.73 with a trick not to save rasters in tmp dir)
rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE)

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
ddeg <- raster(file.path(path.to.expl.var, "tave10_esri", "ddeg.grd"), crs = proj)
## merge all cliimatic variables
expl.stk <- stack(ddeg, subset(bio, c(6, 10, 18, 19)))

## reproject the explanatory variables to work in the right projection system
## load teh ref mask
ras.ref <- raster(ras.ref.file)
expl.stk <- projectRaster(expl.stk, ras.ref)

## do projections --------------------------------------------------------------
##define the projection name
if(grepl("Current", path.to.expl.var)){
  bm.proj.name <- "pure_climat_current"
} else{
  bm.proj.name <- paste0("pure_climat_", sub("/", "_", sub("^.*Full_arctic_30_north/", "", path.to.expl.var))) 
}

## do single models projections
bm.mod.proj <- BIOMOD_Projection(modeling.output = bm.mod,
                                 new.env = stack(expl.stk),
                                 proj.name = bm.proj.name,
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

# ## create the parameter files for the grid -------------------------------------
# 
# ## on idiv_cluster
# out.dir <- "/work/georges/BRISCA/grid_params/"
# dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
# # sp.list <- read.table("~/BRISCA/briscahub/data/sp.list_08102015_red.txt",
# #                       sep = "\t", stringsAsFactors = FALSE, header  = TRUE)
# # sp.list <- sp.list$Biomod.name
# # sp.list <- list.files("/work/georges/BRISCA/Biomod_pure_climate_usgs_no_flaws/")
# sp.list <- list.files("/work/georges/BRISCA/Biomod_pure_climate_strange_distrib/")
# 
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
# path.to.expl.var <- c(path.to.cur.expl.var, path.to.fut.expl.var)
# 
# params <- expand.grid(sp.list = sp.list,
#                       path.to.expl.var = path.to.expl.var)
# 
# write.table(params, file = file.path(out.dir, "params_spcp.txt"), sep = " ",
#             quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)
# 
# ## subselect a part of params?
# params <- params[grepl("csiro_mk360", params$path.to.expl.var), ]
# write.table(params, file = file.path(out.dir, "params_csiro.txt"), sep = " ",
#             quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)
