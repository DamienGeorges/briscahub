##' ----------------------------------------------------------------------------
##' @title Convex hull computation for our species of interest
##' @date 13/10/2015
##' @author Gilles D., Anne O.B. and Damien G.
##' @note This script is based on the IUCN Convexe Hull calculation extracted and
##'   addapted by Gilles.
##' @description The aim of this script is to build the full convex hull for all 
##'   BRISCA species to be able to filter current and future (via migration rate)
##'   SDM projections  
##' @licence Ask Gilles/Anne before reuse.
##' ----------------------------------------------------------------------------

## -- Start of script ----------------------------------------------------------
rm(list = ls())
path.to.briscahub <- "~/Work/BRISCA/briscahub" ## path to teh dir where vbrscahub repos have been cloned
path.to.full.occ.dir <- "" ## path to the dir where all occurences for scpecies are stored
out.dir <- "~/Work/BRISCA/workdir/brsica_shrubs_convex_hull"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## load IUCN_eval Gilles' function
source(file.path(path.to.briscahub, "scripts", "0_data_formatting", "IUCN_eval.r"))

## -- load the reference data --------------------------------------------------
sp.dat <- read.table(file.path(path.to.briscahub, "data/sp.list_08102015_red.txt"), 
                     header = TRUE, sep = "\t", stringsAsFactor = FALSE)

sp.list <- unique(sp.dat$Genus.species)

## loop over species 
for(spe in sp.list){
  occ.dat <- read.table()
  
}




# 
# ### Examples of using IUCN_eval function
# ### Drawing the map.
# IUCN_eval(DATA, country_map=africa, Cell_size=2, DrawMap=T)
# ### A pdf file should be created in the working directory
# 
# IUCN_eval(DATA, country_map=africa, Cell_size=2, DrawMap=T, add.legend=T)
# 
# 
# ### Drawing the map witout adding the legend
# IUCN <- IUCN_eval(DATA, Cell_size=2, DrawMap=F, add.legend=F)
# p1 <- IUCN[[2]]
# 
# #### You add a buffer of width to your convex hull
# p1_Buffered <- gBuffer(p1[[1]], width = 5)
# 
# 
# ### You can mask the predictors according to the convex_hull+buffer
# mask(x = z[[i]], mask = p1_Buffered)
# 
# 
# 
# 






