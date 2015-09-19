################################################################################
##' @title build the models for our selection of shrubs species
##'
##' @author Damien G. 
##' @date 03/08/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   This script will produce species distribution models and ensemble models
##'   based only on climatic variables (here : ddeg, bio_6, bio_10, bio_18 and 
##'   bio_19). Models will be based on 6 algo ('GAM', 'MAXENT', 'GBM', 'RF', 
##'   'MARS', 'CTA'). Presences and pseudo=absences have been manually extracted
##'   in a previous step.
##'   
##'   This scipt need the species name as input argument and should be launch like :
##'     R CMD BATCH "--args Alnus_incana" --vanilla --no-restore --no-save pure_climat_modelling.R
##'     "C:\Program Files\R\R-3.2.0\bin\R.exe" CMD BATCH "--args Betula_pubescens" --vanilla --no-restore --no-save "V:\aa_BRISCA\SDM_sessions\R.scripts\shrubs_pure_climat_modelling.R" "J:\People\Anne\Rout\1_modelling\Betula_pubescens_modelling.Rout"  
##'     "C:\Program Files\R\R-3.2.0\bin\R.exe" CMD BATCH "--args Ribes_spicatum" --vanilla --no-restore --no-save "V:\aa_BRISCA\SDM_sessions\R.scripts\shrubs_pure_climat_modelling.R" "J:\People\Anne\Rout\1_modelling\Ribes_spicatum_modelling.Rout"  
##'     "C:\Program Files\R\R-3.2.0\bin\R.exe" CMD BATCH "--args Salix_berberifolia" --vanilla --no-restore --no-save "V:\aa_BRISCA\SDM_sessions\R.scripts\shrubs_pure_climat_modelling.R" "J:\People\Anne\Rout\1_modelling\Salix_berberifolia_modelling.Rout"  
##'     
##'   A piece of code to generate parameters is available at the end of this script.
##'   A piece of code to generate serie of batch files is also available at the end of this script.
##' 
##' @log
##'   - 09/09/2015: update the script to fit with idiv cluster
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
  out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_wm"
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
require(rgdal)
# require(biomod2, lib.loc="/gpfs0/home/georges/R/old_biomod2")

## load climatique variables ---------------------------------------------------

## define the projection system
proj <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 

## bioclimatic variables
bio <- stack(file.path(in.clim, "bioproj.grd"))
## degree day
ddeg <- raster(file.path(in.gdd, "ddeg"), crs = proj)
## merge all cliimatic variables
clim.cur <- stack(ddeg, subset(bio, c(6, 10, 18, 19)))

## load presences-absences data for our species --------------------------------

## get species presences/pseudo-absences .csv files
pres.thin.file <- list.files(in.spp, 
#                              pattern = paste0("^pres_and_PA_thin_", sp.name, ".csv"),
                             pattern = paste0("^pres_and_10000_PA_thin_", sp.name, ".csv"),
                             full.names = TRUE)

## load the .csv
pres.thin <- read.csv(pres.thin.file)

## reformat input data to be suitable with BIOMOD_FormatingData() --------------

## create response variable
## check if the point is a presence or a pseudo absence
sp.resp <- apply(pres.thin[, paste0("thin.run.", 1:10)], 1, 
                 function(x) any(x == 1, na.rm = TRUE))
## convert into 1/0/NA
sp.resp <- as.numeric(sp.resp)
sp.resp[!sp.resp] <- NA

## extract coordinates
sp.coord <- pres.thin[, c("x", "y")]

## extract climatic variables
sp.env <- extract(clim.cur, sp.coord)

## reformat pseudo-absences table (code 0/1 as TRUE and NA as FALSE)
sp.pa.tab <- apply(pres.thin[, paste0("thin.run.", 1:10)], 2,
                   function(x) !is.na(x))

## keep only the fully defined env cells
to.keep <- apply(sp.env, 1, 
                 function(x) all(!is.na(x)))
sp.resp <- sp.resp[to.keep]
sp.coord <- sp.coord[to.keep, ]
sp.env <- sp.env[to.keep, , drop = FALSE]
sp.pa.tab <- sp.pa.tab[to.keep, , drop = FALSE]

## clean workspace to keep only what we need -----------------------------------

rm(pres.thin.file, pres.thin, bio, ddeg)

## format the data: BIOMOD_Formating() -----------------------------------------

bm.dat <- BIOMOD_FormatingData(resp.var = sp.resp,
                               expl.var = sp.env,
                               resp.xy = sp.coord,
                               resp.name = sp.name,
                               PA.strategy = 'user.defined', 
                               PA.table = sp.pa.tab, 
                               na.rm = TRUE)

## define models options: BIOMOD_MOdellingOptions ------------------------------

bm.opt <- BIOMOD_ModelingOptions(
  MAXENT = list( path_to_maxent.jar = path_to_maxent.jar,
                 maximumiterations = 200,
                 visible = FALSE,
                 linear = TRUE,
                 quadratic = TRUE,
                 product = F,
                 threshold = F,
                 hinge = TRUE,
                 lq2lqptthreshold = 80,
                 l2lqthreshold = 10,
                 hingethreshold = 15,
                 beta_threshold = -1,
                 beta_categorical = -1,
                 beta_lqp = -1,
                 beta_hinge = -1,
                 defaultprevalence = 0.5),
  GAM = list( algo = 'GAM_mgcv', ## note: was mgcv but seems to cause a memory leak
              type = 's_smoother',
              interaction.level = 0,
              myFormula = NULL,
              family = 'binomial'),
  GBM = list( distribution = 'bernoulli',
              n.trees = 1000,
              interaction.depth = 7,
              n.minobsinnode = 5,
              shrinkage = 0.001,
              bag.fraction = 0.5,
              train.fraction = 1,
              cv.folds = 3,
              keep.data = FALSE,
              verbose = FALSE,
              perf.method = 'cv'),
  RF = list( do.classif = TRUE,
             ntree = 500,
             mtry = 'default',
             nodesize = 5,
             maxnodes = NULL),
  MARS = list( degree = 2,
               nk = NULL,
               penalty = 2,
               thresh = 0.001,
               prune = TRUE),
  CTA = list( method = 'class',
              parms = 'default')
)

## launch the modelling procedure: BIOMOD_Modeling() --------------------------

bm.mod <- BIOMOD_Modeling(
  bm.dat,
  models = c('GAM', 'GBM', 'RF', 'MARS', 'CTA', 'MAXENT'), ## MAXENT was removed because it was crashing on Idiv clusters
  models.options = bm.opt,
  NbRunEval = 10,
  DataSplit = 80,
  Yweights = NULL,
  Prevalence = .5,
  VarImport = 0, #3, ## was too long to compute
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = FALSE,
  do.full.models = TRUE,
  modeling.id = "pure_climat")

gc()
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

# ## create the parameter files for the grid -------------------------------------
# 
# out.dir <- "/work/georges/BRISCA/grid_params"
# dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
# ## get all species names
# ## get all .csv files
# pres.thin.files <- list.files(in.spp, pattern = "^pres_and_10000_PA_thin_.*.csv$", full.names = TRUE)
# ## decuce the species names from list of thin files
# sp.list <- sub("^pres_and_10000_PA_thin_", "", tools::file_path_sans_ext(basename(pres.thin.files)))
# 
# params <- data.frame(sp.name = sp.list)
# 
# write.table(params, file = file.path(out.dir, "params_pcm.txt"), sep = " ", 
#             quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)


# ## create a set of batch files for parallel computing over our Win machine -----
# 
# ## define the number of batch files we want to create
# in.spp <- "V:\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Presence-PseudoAbsence_thinned\\Data_output\\gbif_biosc_hultBuff"
# nb.batch <- 25 
# ## get all species names
# ## get all .csv files
# pres.thin.files <- list.files(in.spp, pattern = "^pres_and_PA_thin_.*.csv$", full.names = TRUE)
# ## decuce the species names from list of thin files
# sp.list <- sub("^pres_and_PA_thin_", "", tools::file_path_sans_ext(basename(pres.thin.files)))
# sp.id <- rep(1:nb.batch, length.out = length(sp.list))
# 
# for(i in unique(sp.id)){
#   cat(paste0('"C:/Program Files/R/R-3.2.0/bin/R.exe" CMD BATCH "--args ',
#   sp.list[sp.id == i], 
#   '" --vanilla --no-restore --no-save "V:/aa_BRISCA/SDM_sessions/R.scripts/shrubs_pure_climat_modelling.R" "J:/People/Anne/Rout/1_modelling/',
#   sp.list[sp.id == i], '_modelling.Rout"'), 
#   file = paste0("J:\\People\\Anne\\workdir\\Biomod_pure_climate_batch\\batch_shrubs_pure_climat_modelling", i,".bat"),
#   sep = "\n")
# }
