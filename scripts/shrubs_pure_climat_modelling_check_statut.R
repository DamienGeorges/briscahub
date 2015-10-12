################################################################################
##' @title check that all models have been correctly computed
##' @author Damien G. 
##' @date 18/09/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   This script will check that all the models succedded
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

modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_usgs/"
sp.tab <- read.table("/work/georges/BRISCA/grid_params/params_spcm.txt", header = FALSE, sep = " ")
sp.list <- as.character(sp.tab[, 2])

## update sp.list to fit with the well formated names
## remove all 'tricky' characters from sp names
sp.list <- gsub("-", "", sp.list)
sp.list <- gsub(" ", ".", sp.list, fixed = "TRUE")
sp.list <- gsub("_", ".", sp.list, fixed = "TRUE")

## check if species have been completed
sp.ok <- sapply(sp.list, function(spp){
  file.exists(file.path(modelling.dir, spp, "bm.eval.txt"))
  })

sum(sp.ok)
sp.nok.id <- which(!sp.ok) 
sp.nok.id

## check if at least the modelling part have been completed
sp.mok <- sapply(sp.list, function(spp){
  file.exists(file.path(modelling.dir, 
                        spp,
                        paste0(spp, ".pure_climat.models.out")))
})
sum(sp.mok)
sp.nmok.id <- which(!sp.mok) 
sp.nmok.id

## try to see why it fails
# sp.name <- "Artemisia_sericea" ## 2 occurances
# sp.name <- "Larix_cajanderi" ## 2 occurances
sp.name <- "Salix_arbutifolia" ## 2 occurances
in.spp <- "/data/idiv_sdiv/brisca/SDM_sessions/Presence-PseudoAbsence_thinned/Data_output/gbif_biosc_hult_thined_10000" 
## get species presences/pseudo-absences .csv files
pres.thin.file <- list.files(in.spp, 
                             #                              pattern = paste0("^pres_and_PA_thin_", sp.name, ".csv"),
                             pattern = paste0("^pres_and_10000_PA_thin_", sp.name, ".csv"),
                             full.names = TRUE)

## load the .csv
pres.thin <- read.csv(pres.thin.file)
summary(pres.thin)
apply(pres.thin[, 4:13], 2, sum, na.rm = TRUE)
## end try to see why it fails

## define the list of species we want to rerun from ensemble modeliing part
sp.to.redo <- sp.mok & !sp.ok
sp.to.redo.id <- which(sp.to.redo)

## create the parameter file corresponding to this list of species
out.dir <- "/work/georges/BRISCA/grid_params"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
params <- data.frame(sp.name = names(sp.to.redo.id))

write.table(params, file = file.path(out.dir, "params_pcmR.txt"), sep = " ", 
            quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)
