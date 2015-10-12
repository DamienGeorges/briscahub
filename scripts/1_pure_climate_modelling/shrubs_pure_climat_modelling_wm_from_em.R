################################################################################
##' @title restart to build the models that failed from ensemble modelsin
##'
##' @author Damien G. 
##' @date 03/08/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   Cause a couple off models seems to have failed during the computation,
##'   we will rerun them from the ensemble modelling point.
##' 
##' @log
##' 
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
sp.name <- as.character(args[1])

## test ===
## sp.name <- "Alnus_incana"
## sp.name <- "Artemisia_comata"
## sp.name <- "Betula_pubescens"
## sp.name <- "18"

## definig the machine where the script will run ----------------------------------------
host = "idiv_cluster"

## input/output directories depending on the host ------------------------------
if(host == "pinea"){
  # presences-absences tables
  in.spp <- "~/Work/SHRUBS/WORKDIR/SDM/Damien_ModellingData"
  # worldclim layers
  in.clim <- "~/Work/SHRUBS/WORKDIR/SDM/Damien_ModellingData" 
  # GDD layer
  in.gdd <- "~/Work/SHRUBS/WORKDIR/SDM/Damien_ModellingData"
  # output directory (= workking directory)
  out.dir <- "~/Work/SHRUBS/WORKDIR/SDM/Biomod_pure_climate_highmem"
  # path to maxent.jar file
  path_to_maxent.jar <- "~/Work/SHRUBS/WORKDIR/SDM"
} else if(host == "brisca_cluster"){
  # presences-absences tables
  ##in.spp <- "I:\\C_Write\\Signe\\aa_BRISCA\\SDM_sessions\\Presence-PseudoAbsence_thinned\\Data_output\\gbif_biosc_hult_thined_10000" #or gbif_biosc_hult_random for the random datasets (10,000 PA)
  ## temp for devel
  in.spp <- "V:\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Presence-PseudoAbsence_thinned\\Data_output\\gbif_biosc_hult_thined_10000"
  # worldclim layers
  in.clim <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\Climate\\Macroclimate\\Current\\Processed\\Projected\\bio" 
  # GDD layer
  in.gdd <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\Climate\\Macroclimate\\Current\\Processed\\Projected\\tave10_esri"
  # output directory (= workking directory)
  ##out.dir <- "I:\\C_Write\\Signe\\aa_BRISCA\\SDM_sessions\\Biomod_pure_climate"
  out.dir <- "J:\\People\\Anne\\workdir\\Biomod_pure_climate"
  # path to maxent.jar file  
  path_to_maxent.jar <- "I:\\C_Write\\Signe\\aa_BRISCA\\SDM_sessions\\Maxent"
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
  sp.tab <- read.table("/work/georges/BRISCA/grid_params/params_pcmR.txt", header = FALSE, sep = " ")
  sp.name <- as.character(sp.tab[sp.id, 2])
}

## create the output directory and change the working directory ----------------
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
setwd(out.dir)

## require libraries -----------------------------------------------------------
require(biomod2)
require(rgdal)

## load models already computed
bm.mod.name <- load(file.path(sp.name, paste0(sp.name, ".pure_climat.models.out")))
bm.mod <- get(bm.mod.name)
rm(list = bm.mod.name)

## produce ensemble models: BIOMOD_EnsembleModeling() --------------------------

bm.ensmod <- BIOMOD_EnsembleModeling(modeling.output = bm.mod,
                                     chosen.models = "all",
                                     em.by = 'all',
                                     eval.metric = c('TSS'),
                                     eval.metric.quality.threshold = c(.5),
                                     models.eval.meth = c('TSS','ROC'),
                                     prob.mean = FALSE,
                                     prob.cv = TRUE,
                                     prob.ci = FALSE,
                                     prob.ci.alpha = 0.05,
                                     prob.median = TRUE,
                                     committee.averaging = TRUE,
                                     prob.mean.weight = TRUE,
                                     prob.mean.weight.decay = 'proportional',
                                     VarImport = 0)
gc()

## save other outputs of interest ----------------------------------------------
bm.eval <- rbind(get_evaluations(bm.mod, as.data.frame = TRUE),
              get_evaluations(bm.ensmod, as.data.frame = TRUE))

# bm.vi <- as.data.frame(get_variables_importance(bm.mod))

write.table(bm.eval, file = file.path(out.dir, bm.dat@sp.name, "bm.eval.txt"))
# write.table(bm.vi, file = file.path(out.dir, bm.dat@sp.name, "bm.vi.txt"))

## we will just keep here the models scores and models variables importance


## quit the script -------------------------------------------------------------

quit("no")
