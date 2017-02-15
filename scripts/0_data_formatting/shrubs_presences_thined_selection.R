##########################################################################################################################################################
##################################################---Thinning process of presence data---############################################################################################
#########################################################################################################################################################
rm(list = ls())

## set the paths in relation to the computer we use

# ## on Signe's Cluster
# nb_cores <- 22
# path.to.C_write <- "I:\\C_Write\\" 
# path.to.Rlibs <- "J:\\People\\Damien\\RLIBS"

## on Damien's IARC PC
nb_cores <- 4
path.to.C_write <- "Z:\\"
path.to.Rlibs <-  "C:/Program Files/R/R-3.3.1/library"

setwd(paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\R_workspace"))
#setwd("~/SHRUBS/WORKDIR/SDM/")


.libPaths(path.to.Rlibs)
library(raster)
library(dplyr)



inras <- c(paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\bb_HULTEN\\hult.spp.data\\All.Hulten.rasters\\rasters.all.combi\\"), ## hultens for shrubs
           paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\bb_HULTEN\\hult.spp.data\\Hulten.rasters.trees\\Rasters.combi.reclass.img\\"), ## hultens for trees
           paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\bb_USGS\\usgs.spp.data\\USGS.rasters.trees\\Rasters.raw.img\\") ) ## usgs for trees
 
# inpath <- paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined.all.sources\\")
inpath <- paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined.all.sources.no.flaws\\")
outpath <- paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined.all.sources.no.flaws.hult_usgs.masked\\") 

## define a couple of constant that will be used latter on
buff.dist.hult = 50000 ## the distance use to filter Hulten rasters (hulten points too close to 'TRUE' occurrences will be removed)
buff.dist.thin = 50000 ## the distance use to apply thining procedure
nb.run.thin = 10 ## the nuber of thining repetition perform 

## we will consider the shrub and tree species in the Arctic (PAF ==1) that have not been processed yet (Rerun == 1)
# dataset <- read.table(paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\sp.list_06102015.txt"), sep="\t", header = TRUE, stringsAsFactors = FALSE)
dataset <- read.table(paste0(path.to.C_write, "Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\sp.list_22.12.2016.txt"), sep="\t", header = TRUE, stringsAsFactors = FALSE)

# Subset to only wiev the species with at least one source data
data <- dataset %>% filter(data == 1, rerun1.leave.out0 == 1)

#Extract the species list for which we need to rerun some processes
sp.list <- data %>% select(Genus.species) %>% distinct %>% unlist
sp.list

##' @note
##' !!TRICKY PART!! : 
##' cause we already computed thined dataset for couple of species and we already started to use them
##' for model species distribution, we don't want to redo the job so this species will be removed from
##' species list. In case you want to regenerate the full set of thined dataset you will have to comment
##' the next few lines
# sp.list <- sp.list[sapply(sp.list, function(sp.n){
#   !any(grepl(gsub(" ", "_", sp.n),                                                     
#              list.files(file.path(c(outpath.shrub, outpath.tree), "gbif_biosc_hultBuff_thinned"))))                                     
# })]                                                                                                
                                                                                                     
## Rasters of the hulten rangemaps
ras.files <- list.files(inras, pattern = ".img$", full.names = TRUE)

## define a reference raster to get raster grid
ras.ref <- raster(ras.files[1])
ras.ref[] <- NA

##' @name thining.raster
##' @description randomly select points spaced by a minimum distance
##' 
##' @param ras: a raster with 1 on the set of cells we whant to apply thining and NA every where else
##' @param buff.dist: numeric, the minimal distance separating 2 selected points (in metters for unprojected raster or in projection units for projected ones)
##' @param nb.pts: the number of points randomly seleced at each time step (see details)
##' @param as.raster: logical, should the function return a raster or a data.frame
##' @param lonlat: logical, is the raster system coordinate is in long lat (wgs84) (note this param is the same than the one from raster::pointDistance function)
##' 
##' @details nb.pts argument is a key point for optimasing the processing time. 
##'   At each time step we will select nb.pts in the available cells and calculate
##'   a distance matrix btw all this points. Increasing nb.pts will reduce the number 
##'   of time step needed to complete the sampling (gain of computing time) but will 
##'   increase the memory consumtion and the time use to calculate matrice distances.
##' 
##' @return a 3 column data.frame with the cell number, and associated coordiates of
##'   selected points after thining process or a raster with 1 in selected cells
##'   and NA everywhere else
##'   
##' @example 
##'   ## to do (damien) 
##'   
##' @author damien g.
##' 
thining.raster <- function(ras, buff.dist = 50000, nb.pts = 100, as.raster = FALSE, lonlat = TRUE){
  ## define the objact containing list of thining points
  pts.thin <- data.frame()
  ## define the potential area where we will select presences
  pot.occ.ras <- ras
  ## calculate the number of cancdidates to be thined
  nb.cells.remain <- sum(!is.na(pot.occ.ras[]))
  while(nb.cells.remain){
    ## randomly sample some points in the potential occurence raster
    pts.tmp <- sampleRandom(pot.occ.ras, size = nb.pts, replace = FALSE, cells = TRUE, xy = TRUE)
    ## calculate thedistance btw selected points
    pts.tmp.dist <- pointDistance(pts.tmp[, c('x', 'y')], lonlat = lonlat)
    ## remove the diagonal and upper triangle from distance matrix
    pts.tmp.dist[upper.tri(pts.tmp.dist, diag = TRUE)] <- Inf
    ## keep only pts having a distance greater than the given distance
    pts.tmp <- pts.tmp[apply(pts.tmp.dist > buff.dist, 1, all, na.rm = TRUE), , drop = FALSE]
    ## add the selected points to global list of selected points
    pts.thin <- rbind(pts.thin, pts.tmp[, 1:3])
    ## create the tmp points buffer mask
    pts.buff.tmp <- ras
    pts.buff.tmp[] <- NA ## her we create an empty raster
    pts.buff.tmp[pts.tmp[, 'cell']] <- 1 ## here we add presences from this run in the raster
    pts.buff.tmp <- buffer(pts.buff.tmp, width = buff.dist) ## here we apply a buffer aroud selected points
    ## mask the potential occurence raster (remove some candidates)
    pot.occ.ras <- mask(pot.occ.ras, pts.buff.tmp, inverse = TRUE)
    ## update the number of cells remaining
    nb.cells.remain <- sum(!is.na(pot.occ.ras[]))
    ## print the function progress
    cat(sprintf('\r%d candidate cells remains...', nb.cells.remain))
  }
  cat("\n> completed!", nrow(pts.thin), "thined cells selected!\n")
  ## return the output in the appropriate format
  if(as.raster){
    ras.thin <- ras
    pts.buff.tmp[] <- NA ## her we create an empty raster
    pts.buff.tmp[pts.thin[, 'cell']] <- 1 ## here we add presences from this run in the raster
    return(ras.thin)
  } else {
    return(pts.thin)
  } 
}



# Here I try to mask out the Hulten presences in the vacinity of GBIF or BIOSCIENCE presences (50km) and only extracting the ones falling outside the buffer
## test 
## k = 1

## loop over species

## sequential version
##for (k in 1:length(sp.list)){ 
## end sequential version

## parallel version
library(foreach)
library(doParallel)
cl <- makeCluster(nb_cores)
registerDoParallel(cl)
# pass libPath to workers, NOTE THIS LINE
clusterCall(cl, function(x) .libPaths(x), .libPaths())

foreach(k = 1:length(sp.list), .packages = c('raster', 'dplyr')) %dopar% {
## end parallel version
  # .libPaths("J:\\People\\Damien\\RLIBS")
  # library('raster')
  # library('dplyr')
  
  ## read the occurence table for the species
  dat <- read.table(file.path(inpath, paste0(sp.list[k], ".txt")), 
                    sep="\t", header = TRUE, fill= TRUE, stringsAsFactors = FALSE)
  
  ##' 
  ##' @note this part is not required anymore in the latest verison because the hulten and ugs data have been 
  ##' extracted by Anne for the 22 remaining species
  ##' 
  # ## does the species has hulten or usgs data?
  # has.hulten.usgs <- any(grepl(paste0(sp.list[k], ".img"), ras.files))
  # dat1 <- dat %>% filter(!is.na(X), !is.na(Y), !is.element(Source, c("Hulten", "USGS")))
  # file.name <- file.path(outpath, "gbif_biosc_hultBuff", paste0(sub(" ", "_", sp.list[k]), ".csv"))
  # dir.create(dirname(file.name), showWarnings = FALSE, recursive = TRUE)
  # 
  # if (nrow(dat1) == 0 | !has.hulten.usgs){ ## either only hulten or no hulten points case
  #   write.csv(dat, file.name, row.names = FALSE)
  # } else { ## hulten and another source of points
  #   pt.mask <- SpatialPoints(dat1[, c("X", "Y")], 
  #                            proj4string = CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  #   ## add the points on a raster
  #   pt.ras <- ras.ref
  #   pt.ras[cellFromXY(pt.ras, pt.mask)] <- 1
  #   
  #   ## create a 50km buffer aroud all presences points (where we don't want to sample hulten presences)
  #   buff.ras <- buffer(pt.ras, width = buff.dist.hult)
  #   ## load the hulten raster for our species 
  #   hult.ras <- raster(grep(sp.list[k], ras.files, value = TRUE))
  #   ## remove all area in the buffer
  #   hult.ras.masked <- mask(hult.ras, buff.ras, inverse = TRUE)
  #   ## remove all 'absences'
  #   hult.ras.masked[hult.ras.masked != 1] <- NA
  #   ## convert all potential Hulten cells into points
  #   hult.ras.masked.pts <- as.data.frame(rasterToPoints(hult.ras.masked))
  #   hult.ras.masked.pts <- data.frame( X = hult.ras.masked.pts$x,
  #                                      Y = hult.ras.masked.pts$y,
  #                                      Source = "Hulten" )
  #   ## reintegrate the hulten points into the dataset
  #   ##' @note here I removed the column with the species names
  #   dat2 <- rbind(dat1[, c("X", "Y", "Source")], hult.ras.masked.pts)
  #   ## write a copy of the file on the hardrive
  #   write.csv(dat2, file.name, row.names = FALSE)
  # }
  
  ##' @note At this stage we have a table with our conbined occurences for 
  ##'   GBIF, Bioscience and Hulten (out of the 50km buffer). We will then
  ##'   make a subselection of this occurences with SpThin like utilities
  
  ## define a full presences mask that will be use to do thining
  # full.pts.df <- read.csv(file.name, header = TRUE)
  full.pts.df <- dat
  full.pts.ras <- raster(ras.files[1])
  full.pts.ras[] <- NA
  full.pts.ras[cellFromXY(full.pts.ras, full.pts.df[, c("X", "Y")])] <- 1
  
  
  #   full.pts.ras <- calc(stack(pt.ras, hult.ras.masked), fun = sum, na.rm = TRUE)
  #   full.pts.ras[full.pts.ras == 0] <- NA
  ## apply the home made thining function
  thin.pts.list <- lapply( 1:nb.run.thin, function(x){
    thin.df <- thining.raster( full.pts.ras, 
                               buff.dist = 50000, 
                               nb.pts = 200, 
                               as.raster = FALSE, 
                               lonlat = FALSE)
    thin.df[[paste0("thin.run.", x)]] <- 1
    return(thin.df)} )
  ## merge the thined pts
  thin.pts <- Reduce(function(...) merge(..., all = T, by = c("cell", "x", "y") ), thin.pts.list)
  ## save the thined dataset
  file.name <- file.path(outpath, "gbif_biosc_hultBuff_thinned", paste0("pres_thin_", sub(" ", "_", sp.list[k]), ".csv"))
  dir.create(dirname(file.name), showWarnings = FALSE, recursive = TRUE)
  write.csv(thin.pts, file.name, row.names = FALSE)
}

stopCluster(cl)
## exit the script
# q('no')