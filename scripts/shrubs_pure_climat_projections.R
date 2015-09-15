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
##'     - ~ 150 shrubs and tree species
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
## job.id <- 1


## definig the machine where the script will run -------------------------------
host = "pinea"

## input/output directories depending on the host ------------------------------
if(host == "pinea"){
  # path to biomod2 model output directory (= working directory)
  in.mod <- "/media/georgeda/DamienPersonalDrive/Aarhus/aa_BRISCA/SDM_sessions/Biomod_pure_climate"
  # worldclim layers
  in.clim <- file.path("/media/georgeda/DamienPersonalDrive/Aarhus/aa_BRISCA/Data/Climate/Macroclimate/",
  )
  # GDD layer
  in.gdd <- "~/Work/SHRUBS/WORKDIR/SDM/Damien_ModellingData"
  # output directory (= workking directory)
  out.dir <- "~/Work/SHRUBS/WORKDIR/SDM/Biomod_pure_climate"
  # path to maxent.jar file
  path_to_maxent.jar <- "~/Work/SHRUBS/WORKDIR/SDM"
} else if(host == "brisca_cluster"){
  ## TODO (Damien)
} else if (host == "idiv_cluster"){
  # presences-absences tables
  in.spp <- "/data/idiv_sdiv/brisca/SDM_sessions/Presence-PseudoAbsence_thinned/Data_output/gbif_biosc_hult_thined_10000" 
  # worldclim layers
  in.clim <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Current/Processed/Projected/bio"
  # GDD layer
  in.gdd <- "/data/idiv_sdiv/brisca/Data/Climate/Macroclimate/Current/Processed/Projected/tave10_esri"
  # output directory (= workking directory)
  out.dir <- "/work/georges/BRISCA/Biomod_pure_climate"
  # path to maxent.jar file  
  path_to_maxent.jar <- "/data/idiv_sdiv/brisca/SDM_sessions/Maxent"
  ##' @note beacause of the the job manager installed in this cluster (qsub)
  ##'   the input argument is the species ID not the species name so we need 
  ##'   to recover species name manually
  sp.id <- as.numeric(sp.name)
  sp.tab <- read.table("/work/georges/BRISCA/grid_params/params_pcm.txt", header = FALSE, sep = " ")
  sp.name <- as.character(sp.tab[sp.id, 2])
}

## create the output directory and change the working directory ----------------
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
setwd(out.dir)

## require libraries -----------------------------------------------------------
require(biomod2)

quit('no')

## end of script ---------------------------------------------------------------

## create the parameter files for the grid -------------------------------------

# out.dir <- "/work/georges/BRISCA/grid_params"
## on pinea
out.dir <- "~/Work/BRISCA/grid_params"
in.spp <- "/media/georgeda/DamienPersonalDrive/Aarhus/aa_BRISCA/SDM_sessions/Presence-PseudoAbsence_thinned/Data_output/gbif_biosc_hult_thined_10000"

dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## get all species names from input presence absences input data
pres.thin.files <- list.files(in.spp, 
                              pattern = "^pres_and_10000_PA_thin_.*.csv$", full.names = TRUE)
## decuce the species names from list of thin files
sp.list <- sub("^pres_and_10000_PA_thin_", "", 
               tools::file_path_sans_ext(basename(pres.thin.files)))
## convert the name to fit with biomod2 species name format
sp.list <- gsub("_", ".", sp.list, fixed = TRUE)

## define the gcm and rcp we want to consider
rcp.list <- c("RCP_2.6_2080", "RCP_4.5_2080", "RCP_6.0_2080", "RCP_8.5_2080")
gcm.list <- c("cesm1_cam5", "gfdl_esm2m", "miroc_miroc5", "mri_cgcm3", "ncar_ccsm4",
              "nimr_hadgem2ao")
rcp.gcm.comb <- expand.grid(rcp.list = rcp.list,
                            gcm.list = gcm.list)
path.to.rep.var <- paste("")

params <- expand.grid(sp.list = sp.list,
                      rcp.list = rcp.list,
                      gcm.list = gcm.list)


write.table(params, file = file.path(out.dir, "params_spcp.txt"), sep = " ", 
            quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)
