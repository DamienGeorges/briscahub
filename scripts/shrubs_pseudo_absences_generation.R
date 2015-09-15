### HEADER #####################################################################
##' @title generate pseudo absences for our gbif/bioscience/hulten thinned presences
##'
##' @author Damien G. 
##' @contact damien.georges2 at gmail.com
##' 
##' @date 19/06/2015
##' 
##' @description This script will create for each species a csv file containing
##'   our selected presences and the associated selected pseudo absences.
##'   
##' @note 
##'   this script is based on the outputs of "Hulten.extract.xy_dg_08-06-2015.R"
##'   outputs files. 
##'  
##' 
##' @log
##'   - 08/09/2015 (damien): launch the script to produce a third version of input data
##'     now we should have:
##'       - presences + 10000 random PA
##'       - presences + 10000 thined PA
##'       - presences + 10 times the number of presences PA
##'   - 08/09/2015 (damien): make it parallel via foreach and dopar
##'   - 14/09/2015 (damien): add an optional peace of code to regenerate only non already
##'     existing files (cause we decided to add couple of species and do not want to
##'     erase the PA generated for species we have already do modelling)
##'   - 14/09/2015 (damien): deal with both shrubs and trees
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
## END OF HEADER ###############################################################


# init the workspace ------------------------------------------------

rm(list = ls())
setwd("I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Presence-PseudoAbsence_thinned\\Functions\\")
# setwd("~/Work/SHRUBS/WORKDIR/SDM/")


# load libraries and useful functions -------------------------------------

library(raster)
library(tools)
source("thining_raster.R")


# define paths to data and parameters -------------------------------------

## define the path to directory where all presences thined csv are stored
pres.thin.dir <- c("I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined_hult_masked\\gbif_biosc_hultBuff_thinned\\",
                   "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined_hult_masked_trees\\gbif_biosc_hultBuff_thinned\\")
outpath <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Presence-PseudoAbsence_thinned\\Data_output\\"
# pres.thin.dir <- "."
# outpath <- "."


## define a couple of parameters
ref.grid <- raster("reference_grid.img") ## the reference grid (i.e 10km gridded wold map)
dist.min.from.pres <- 50000 ## the minimal distance from a PA to a presence
dist.max.from.pres <- 500000 ## the maximal distance from a PA to presence
## nb.PA.max <- 10 * sum(dat$thin.run.1, na.rm = TRUE) ## the maximal number of PA.. 
nb.PA.max <- 10000
##' @note nb.PA.max has to be defined in the generate.pseudo.abs() function.
##'   it is set to 10 x the niumber of presence now but should be change for a 
##'   fixed integer.
PA.thin.dist <- 50000 ## the minimal distance btw 2 PA
##' @note by default we select the PA via a thining method (as for presences) but
##'   that should be easily changed to a fully random sampling (un)commenting a 
##'   part of the function code (see generate.pseudo.abs() function note)

## define a output directory that fits with PA samppling parameters define above
## here some proposition
# out.dir <- "gbif_biosc_hult_random" ## presences + 10000 random PA
out.dir <- "gbif_biosc_hult_thined_10000" ## presences + 10000 thined PA
# out.dir <- "gbif_biosc_hultBuff" ## presences + 10 times the number of presences PA

## define a file prefix
# file.pre <- "pres_and_PA_thin_ran" ## presences + 10000 random PA
file.pre <- "pres_and_10000_PA_thin_" ## presences + 10000 thined PA
# file.pre <- "pres_and_PA_thin_" ## presences + 10 times the number of presences PA


# recover needed data and species list ------------------------------------

## get all .csv files
pres.thin.files <- list.files(pres.thin.dir, pattern = "^pres_thin_.*.csv$", full.names = TRUE)
## decuce the species names from list of thin files
sp.list <- sub("^pres_thin_", "", file_path_sans_ext(basename(pres.thin.files)))

##' @note !!TRICKY PART!!
##'   PA selection was done in 2 campain and cause we don't want to overwrite
##'   first campain results we will make a species subselection. If you want to
##'   regenerate all the files just comment the next few lines.
sp.list <- sp.list[sapply(sp.list, function(sp.n){
  !any(grepl(gsub(" ", "_", sp.n), 
             list.files(file.path(outpath, out.dir))))
})]
## ensure that sp.list and pres.thin.files are sorted in a coherent order
pres.thin.files <- sapply(sp.list, function(sp.n){
  grep(sp.n, pres.thin.files, value = TRUE)
})



# define the PA sampling function -----------------------------------------

##' @name generate.pseudo.abs
##' @description generate pseudo absences in a buffer area based on presence distances
##' 
##' @param pres.xy: a 2 column data.frame containing coordinates of presences in the porjection system than ref.grid
##' @param ref.grid: a raster with 1 on the reference area cells (e.g. world map) and NA every where else
##' @param dist.min.from.pres: the minimal distance that should separate a presence to a PA
##' @param dist.max.from.pres: the maximal distance that should separate a presence to a PA
##' @param nb.PA.max: the maximal number of PA we want to sample
##' @param PA.thin.dist: the thining distance we want to use to select our PA
##' 
##' @return a 3 column data.frame with cellid, x and y coordinates of our selected PA
##' 
##' @note you can choose to do a fully random sampling of PA instead of the thining procedure by commenting/uncommenting
##'   indicated peace of code in the function ('PA.thin <- ...')
##'   
##' @example
##'   ## TODO(damien)
generate.pseudo.abs <- function(pres.xy, ref.grid, dist.min.from.pres, dist.max.from.pres, nb.PA.max, PA.thin.dist){
  ## add presences on the grid
  pres.grid <- ref.grid
  pres.grid[] <- NA
  pres.grid[cellFromXY(pres.grid, pres.xy)] <- 1
  ## define buffer aroud presences
  cat("\n> generating presences min distance buffer...")
  pres.grid.buff.min <- buffer(pres.grid, width = dist.min.from.pres)
  cat("\n> generating presences max distance buffer...")
  pres.grid.buff.max <- buffer(pres.grid, width = dist.max.from.pres)
  ## define the area where we will sample PA
  ## we want to be in ref.grid & in pres.grid.buf.max & out of pres.grid.buff.min
  PA.buff <- mask(ref.grid * pres.grid.buff.max, pres.grid.buff.min, inverse = TRUE)
  cat("\n> buffer generation completed.\n")
  ## get pseudo absences in the buffer 
  cat("\n> extracting pseudo absences...\n")
  ## via a thining procedure
#   PA.thin <- thining.raster(ras = PA.buff, 
#                             buff.dist = PA.thin.dist, 
#                             nb.pts = 100, 
#                             as.raster = FALSE, 
#                             lonlat = FALSE, 
#                             nb.thin.pts.max = nb.PA.max)
  ## or via a fully random sampling
  PA.thin <- data.frame(sampleRandom(PA.buff, size = nb.PA.max, replace = FALSE, cells = TRUE, xy = TRUE))[, 1:3]

  return(PA.thin)  
}


# main job: extract pseudo absences for our species ---------------------------------


## loop over species

# ## sequential version
# for (k in 1:length(sp.list)){ 
# ## end sequential version

## parallel version
library(foreach)
library(doParallel)
cl <- makeCluster(20)
registerDoParallel(cl)
foreach(k = 1:length(sp.list)) %dopar% {
## end parallel version

  ## read the occurence table for the species
  dat <- read.csv(pres.thin.files[k])
  ## apply the home made thining function
  thin.pts.list <- lapply( names(dat)[-(1:3)], function(x){
    thin.df <- generate.pseudo.abs(pres.xy = dat[dat[[x]] == 1, c("x", "y")],
                                   ref.grid = ref.grid,
                                   dist.min.from.pres = dist.min.from.pres,
                                   dist.max.from.pres = dist.max.from.pres,
                                   nb.PA.max = nb.PA.max,
                                   PA.thin.dist = PA.thin.dist)
    thin.df[[x]] <- 0
    return(thin.df)} )
  ## merge the thined pts
  thin.pts <- Reduce(function(...) merge(..., all = T, by = c("cell", "x", "y") ), thin.pts.list)
  ## merge thin pts with original data
  thin.pts.all <- rbind(dat, thin.pts) 
  ## save the thined dataset
  file.name <- file.path(outpath, out.dir, paste0(file.pre, sub(" ", "_", sp.list[k]), ".csv"))
  dir.create(dirname(file.name), showWarnings = FALSE, recursive = TRUE)
  write.csv(thin.pts.all, file.name, row.names = FALSE)
  NULL ## for parallel version only
}
