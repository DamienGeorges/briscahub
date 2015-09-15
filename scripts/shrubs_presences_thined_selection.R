
##########################################################################################################################################################
##################################################---Thinning process of presence data---############################################################################################
##########################################################################################################################################################
rm(list = ls())
setwd("I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\R_workspace")
#setwd("~/SHRUBS/WORKDIR/SDM/")

library(raster)
#library(spThin)

inras.shrub <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\bb_HULTEN\\hult.spp.data\\All.Hulten.rasters\\rasters.all.combi\\"
inpath.shrub <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined\\"
outpath.shrub <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined_hult_masked\\" 

inras.tree <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\bb_HULTEN\\hult.spp.data\\Hulten.rasters.trees\\Rasters.combi.reclass.img\\"
inpath.tree <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined_trees\\"
outpath.tree <- "I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\Species.list\\Occurrence.tables.combined_hult_masked_trees\\" 

## define a couple of constant that will be used latter on
buff.dist.hult = 50000 ## the distance use to filter Hulten rasters (hulten points too close to 'TRUE' occurrences will be removed)
buff.dist.thin = 50000 ## the distance use to apply thining procedure
nb.run.thin = 10 ## the nuber of thining repetition perform 

## we will consider the shrub species having for which we have heigth data
MASTER <- read.table("I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\aa_MASTER\\MASTER12.txt", sep="\t", header = TRUE, dec = ".", fill= TRUE, stringsAsFactors = FALSE)
head(MASTER)

Arctic.sub <- subset(MASTER, Arctic.Shrubs == 1)

## Find unique shrub names
Arctic.names <- unique(Arctic.sub$Genus.species)
Arctic.names  #(183)

## Which we have height data for
height.dat <- subset(Arctic.sub, !is.na(Arctic.sub$All.height.median))             

## Find unique shrub names
height.names <- unique(height.dat$Genus.species)
height.names  #(146)

shrub.list <- height.names

## we will qlso consider couple of tree species thqt will be use to shade highest class shrubs
tree.sub <- subset(MASTER, Growthform.manual == "TREE")
tree.list <- unique(tree.sub$Genus.species)

## merge our shrubs and tree lists
sp.list <- unique(c(shrub.list, tree.list))

##' @note
##' !!TRICKY PART!! : 
##' cause we already computed thined dataset for couple of species and we already started to use them
##' for model species distribution, we don't want to redo the job so this species will be removed from
##' species list. In case you want to regenerate the full set of thined dataset you will have to comment
##' the next few lines
sp.list <- sp.list[sapply(sp.list, function(sp.n){
  !any(grepl(gsub(" ", "_", sp.n), 
       list.files(file.path(c(outpath.shrub, outpath.tree), "gbif_biosc_hultBuff_thinned"))))
})]

## Rasters of the hulten rangemaps
ras.files <- list.files(c(inras.shrub, inras.tree), pattern = ".img$", full.names = TRUE)

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
## test k = 1
## loop over species

## sequential version
# for (k in 1:length(sp.list)){ 
## end sequential version

## parallel version
library(foreach)
library(doParallel)
cl <- makeCluster(20)
registerDoParallel(cl)
foreach(k = 1:length(sp.list), .packages = c('raster')) %dopar% {
## end parallel version

  ## get some information on the species
  ## is it a tree or a shrub?
  is.shrub <- is.element(sp.list[k], shrub.list)
  ## does the species has hulten data?
  has.hulten <- any(grepl(paste0(sp.list[k], ".img"), ras.files))
  
  ## adapt the path depending on the type of plant
  if(is.shrub){
    inras <- inras.shrub
    inpath <- inpath.shrub
    outpath <- outpath.shrub
  } else {
    inras <- inras.tree
    inpath <- inpath.tree
    outpath <- outpath.tree
  }
  
  ## read the occurence table for the species
  dat <- read.table(file.path(inpath, paste0(sp.list[k], ".txt")), 
                    sep="\t", header = TRUE, fill= TRUE, stringsAsFactors = FALSE)
  dat1 <- subset(dat, !is.na(dat$X))
  dat1 <- subset(dat1, dat1$Source != "Hulten")
  
  file.name <- file.path(outpath, "gbif_biosc_hultBuff", paste0(sub(" ", "_", sp.list[k]), ".csv"))
  dir.create(dirname(file.name), showWarnings = FALSE, recursive = TRUE)
  
  if (nrow(dat1)==0 | !has.hulten){ ## either only hulten or no hulten points case
    write.csv(dat, file.name, row.names = FALSE)
  } else { ## hulten and another source of points
    pt.mask <- SpatialPoints(dat1[, c("X", "Y")], 
                                  proj4string = CRS("+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
    ## add the points on a raster
    pt.ras <- ras.ref
    pt.ras[cellFromXY(pt.ras, pt.mask)] <- 1
  
    ## create a 50km buffer aroud all presences points (where we don't want to sample hulten presences)
    buff.ras <- buffer(pt.ras, width = 50000)
    ## load the hulten raster for our species 
    hult.ras <- raster(ras.files[k])
    ## remove all area in the buffer
    hult.ras.masked <- mask(hult.ras, buff.ras, inverse = TRUE)
    ## remove all 'absences'
    hult.ras.masked[hult.ras.masked != 1] <- NA
    ## convert all potential Hulten cells into points
    hult.ras.masked.pts <- as.data.frame(rasterToPoints(hult.ras.masked))
    hult.ras.masked.pts <- data.frame( X = hult.ras.masked.pts$x,
                                       Y = hult.ras.masked.pts$y,
                                       Source = "Hulten" )
    ## reintegrate the hulten points into the dataset
    ##' @note here I removed the column with the species names
    dat2 <- rbind(dat1[, c("X", "Y", "Source")], hult.ras.masked.pts)
    ## write a copy of the file on the hardrive
    write.csv(dat2, file.name, row.names = FALSE)
  }

  ##' @note At this stage we have a table with our conbined occurences for 
  ##'   GBIF, Bioscience and Hulten (out of the 50km buffer). We will then
  ##'   make a subselection of this occurences with SpThin like utilities

  ## define a full presences mask that will be use to do thining
  full.pts.df <- read.csv(file.name, header = TRUE)
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

## exit the script
q('no')
