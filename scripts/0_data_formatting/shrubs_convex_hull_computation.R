##' ----------------------------------------------------------------------------
##' @title Convex hull computation for our species of interest
##' @date 13/10/2015
##' @author Damien G. (inspired by Gilles D. and Anne O.B.)
##' @note This script is based on the IUCN Convexe Hull calculation extracted and
##'   addapted by Gilles.
##' @description The aim of this script is to build the full convex hull for all 
##'   BRISCA species to be able to filter current and future (via migration rate)
##'   SDM projections  
##'   3 types of convex hull buffers are constructed so far:
##'     - one by big hand-defined area (eurasia, america, high north)
##'     - one splitting automatically the occuranc e points by a kmean clustering 
##'       at world wide scale (number of group optimize by BIC)
##'     - on using both of the 2 first technics split based on kmean in each of
##'       the hand defined area 
##' @licence GPL
##' ----------------------------------------------------------------------------

## -- Start of script ----------------------------------------------------------
rm(list = ls())

## -- decine some paths depending on the machine used --------------------------
# ## on pinea
# path.to.briscahub <- "~/Work/BRISCA/briscahub" ## path to teh dir where vbrscahub repos have been cloned
# out.dir <- "~/Work/BRISCA/workdir/brsica_shrubs_convex_hull"
## on brsica
path.to.briscahub <- "J:/People/Damien/BRISCA/briscahub" ## path to teh dir where vbrscahub repos have been cloned
out.dir <- "J:/People/Damien/BRISCA/workdir/brsica_shrubs_convex_hull_full_filtered"

# path.to.full.occ.dir <- "I:/C_Write/Signe/aa_BRISCA/Data/StudySpecies/Processed/Species.list/Occurrence.tables.combined.all.sources/" ## path to the dir where all occurences for scpecies are stored
path.to.full.occ.dir <- "I:/C_Write/Damien/BRISCA/workdir/Occurrence.tables.combined.all.sources.no.outliers.merged" ## path to the dir where all occurences for scpecies are stored
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## -- require libraries needed -------------------------------------------------
library(sp) ## for spatial polygons manipulation
library(spatstat) ## for convex hull computation
library(prevR) ## for testing if points fall in a given spatialPolygon
library(maptools) ## to merge spatialPolygons
library(adegenet) ## to split our occurences into groups


## -- load IUCN_eval Gilles' function ------------------------------------------
# source(file.path(path.to.briscahub, "scripts", "0_data_formatting", "IUCN_eval.r"))

## -- define the convex hull polygon function ----------------------------------
##' @note inspired by Gilles D. function
Convex_Hull_Poly <- function(XY, remove.outliers = FALSE, make.group = FALSE, split.area = list(), proj.crs = NULL){
  if(remove.outliers){
    ## determine which points are outliers along X and Y
    bpX <- boxplot(XY[, 1], plot = FALSE) 
    bpY <- boxplot(XY[, 2], plot = FALSE) 
    ## remove outliers
    XY <- XY[(XY[, 1] >= bpX$stats[1, 1] & 
                XY[, 1] <= bpX$stats[5, 1] &
                XY[, 2] >= bpY$stats[1, 1] & 
                XY[, 2] <= bpY$stats[5, 1]), ]
  } ## end remove outliers
  XY$area <- 1 ## the default area
  if(length(split.area)){
    ## add a colum defining which area each point is bellonging to
    for(p.id in 1:length(split.area)){
      cat("\n> constructing convex hull for area", p.id)
      poly.area <- split.area[[p.id]]
      in.poly.area <- point.in.SpatialPolygons(XY[, 1], XY[, 2], poly.area)
      cat("\n  ", sum(in.poly.area, na.rm = T), "points in this area")
      XY$area[in.poly.area] <- p.id + 1
    }
  }
  if(make.group){
    for(ar in unique(XY$area)){
      cat("\n> building groups for area", ar - 1, "...")
      grp <- try(find.clusters(XY[XY$area == ar, , drop = FALSE], 
                               max.n = min(15, sum(XY$area == ar, na.rm = TRUE) %/% 10),
                               n.pca = 10, 
                               scale = FALSE, 
                               choose = FALSE))
      if(!inherits(grp, 'try-error')){
        cat("\n\t", names(grp$stat), "(", grp$size,")")
        XY$area[XY$area == ar] <- 100 * ar + as.numeric(grp$grp)
      }
    }
  }
  ## check that at least 6 points
  low.effective.area <- table(XY$area) < 10
  low.effective.area <- names(low.effective.area)[low.effective.area]
  if(length(low.effective.area)){
    XY <- XY[!is.element(XY$area, low.effective.area), ]
  }
  
  l.hpoly <- lapply(unique(XY$area), function(area.id){
    hpts <- spatstat::convexhull.xy(XY[XY$area == area.id, 1:2])
    return(owin2SP(hpts))
  })
  ## apply the proj.crs if any given
  if(length(proj.crs)){
    l.hpoly <- lapply(l.hpoly, function(x){proj4string(x) <- proj.crs; return(x)})
  }
  return(l.hpoly)
}

## convert a spatstat owin object into a polygon
owin2Polygons <- function(x, id="1") {
  stopifnot(spatstat::is.owin(x))
  x <- spatstat::as.polygonal(x)
  closering <- function(df) { df[c(seq(nrow(df)), 1), ] }
  pieces <- lapply(x$bdry,
                   function(p) {
                     Polygon(coords=closering(cbind(p$x,p$y)),
                             hole=is.hole.xypolygon(p))  })
  z <- Polygons(pieces, id)
  return(z)
}

## convert a spatstat owin object into a sp spatial polygon
owin2SP <- function(x) {
  stopifnot(spatstat::is.owin(x))
  y <- owin2Polygons(x)
  z <- SpatialPolygons(list(y))
  return(z)
}


## -- load the reference data --------------------------------------------------
sp.dat <- read.table(file.path(path.to.briscahub, "data/sp.list_08102015_red.txt"), 
                     header = TRUE, sep = "\t", stringsAsFactor = FALSE)

sp.list <- unique(sp.dat$Genus.species)

## load the polygons for convex hull splitting
(load(file.path(path.to.briscahub, "data/convex_hull_splitting_poly.RData")))

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
  sp.ch.poly.list <- Convex_Hull_Poly(occ.dat[, c("X", "Y")], 
                                 remove.outliers = FALSE, ##TRUE, 
                                 split.area = list(poly.eurasia, poly.high.north, poly.north.america),
                                 proj.crs = crs.laea)
  ## save the object as an R object
  save(sp.ch.poly.list, file = file.path(out.dir, paste0(spe, "_convex_hull_poly_area.RData")))
  
  ## compute the convex hull for our species
  sp.ch.poly.list <- Convex_Hull_Poly(occ.dat[, c("X", "Y")], 
                                      remove.outliers = FALSE, ##TRUE,  
                                      make.group = TRUE,
                                      proj.crs = crs.laea)
  ## save the object as an R object
  save(sp.ch.poly.list, file = file.path(out.dir, paste0(spe, "_convex_hull_poly_group.RData")))
  
  ## compute the convex hull for our species
  sp.ch.poly.list <- Convex_Hull_Poly(occ.dat[, c("X", "Y")], 
                                      remove.outliers = FALSE, ##TRUE, 
                                      split.area = list(poly.eurasia, poly.high.north, poly.north.america),
                                      make.group = TRUE,
                                      proj.crs = crs.laea)
  ## save the object as an R object
  save(sp.ch.poly.list, file = file.path(out.dir, paste0(spe, "_convex_hull_poly_area_group.RData")))
  
}
