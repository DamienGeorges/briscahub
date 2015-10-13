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

## -- decine some paths depending on the machine used --------------------------
# ## on pinea
# path.to.briscahub <- "~/Work/BRISCA/briscahub" ## path to teh dir where vbrscahub repos have been cloned
# out.dir <- "~/Work/BRISCA/workdir/brsica_shrubs_convex_hull"
## on brsica
path.to.briscahub <- "J:/People/Damien/BRISCA/briscahub" ## path to teh dir where vbrscahub repos have been cloned
out.dir <- "J:/People/Damien/BRISCA/workdir/brsica_shrubs_convex_hull"

path.to.full.occ.dir <- "I:/C_Write/Signe/aa_BRISCA/Data/StudySpecies/Processed/Species.list/Occurrence.tables.combined.all.sources/" ## path to the dir where all occurences for scpecies are stored
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## -- require libraries needed -------------------------------------------------
library(sp)
library(grDevice)

## -- load IUCN_eval Gilles' function ------------------------------------------
# source(file.path(path.to.briscahub, "scripts", "0_data_formatting", "IUCN_eval.r"))

## -- define the convex hull polygon function ----------------------------------
##' @note this function comes from Gilles D. function
Convex_Hull_Poly <- function(XY){
  hpts <- chull(XY); hpts <- c(hpts, hpts[1])
  POLY <- "POLYGON(("
  for (i in 1:length(hpts)){
    POLY <- paste(POLY,XY[hpts[i],2]," ",XY[hpts[i],1], sep="")
    if(i!=length(hpts)) POLY <- paste(POLY,", ", sep="")
    if(i==length(hpts)) POLY <- paste(POLY,"))", sep="")
  }
  p1 = readWKT(POLY)  
  return(p1)
}

## -- load the reference data --------------------------------------------------
sp.dat <- read.table(file.path(path.to.briscahub, "data/sp.list_08102015_red.txt"), 
                     header = TRUE, sep = "\t", stringsAsFactor = FALSE)

sp.list <- unique(sp.dat$Genus.species)

## loop over species 
for(spe in sp.list){
  cat("\n> spe:", spe)
  ## load the table where all the species occurences are recorded
  occ.dat <- read.table(file.path(path.to.full.occ.dir, paste0(spe, ".txt")),
                        sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  occ.dat <- na.omit(occ.dat)
  ## occurences are defines in laea proj system
  crs.laea <- CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  ## compute the convex hull for our species
  sp.ch.poly <- Convex_Hull_Poly(occ.dat[, c("X", "Y")])
  proj4string(sp.ch.poly) <- crs.laea
  ## save the object as an R object
  save(sp.ch.poly, file = file.path(out.dir, paste0(spe, "_convex_hull_poly.RData")))
}
