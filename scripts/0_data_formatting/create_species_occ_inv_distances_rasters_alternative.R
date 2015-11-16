##' ----------------------------------------------------------------------------
##' @title Create species distance rasters -- 2nd way to implement the same thing
##' @author damien g.
##' @date 26/10/2015
##' @licence GPL
##' ----------------------------------------------------------------------------

##' @note the XY rasters will be based on the ddeg raster we use for modelling
##'   our species disttributions

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
job.id <- as.numeric(args[1])
# ## TEST
job.id <- 1

## get the list of pixels we want to consider during this job
job.pix.id.table <- read.table("~/BRISCA/WORKDIR/job_pix_id_table.txt", h = TRUE)
cell.ids <- job.pix.id.table$cell.id[job.pix.id.table$job.id == job.id]

# ## on idiv
# param.file <- "/home/georges/BRISCA/briscahub/data/sp.list_08102015_red.txt"
## on luke
param.file <- "/home/dgeorges/BRISCA/briscahub/data/sp.list_08102015_red.txt"
param.tab <- read.table(param.file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

## laad libraries ---------------------------------------------------------------

.libPaths("/home/dgeorges/R/x86_64-pc-linux-gnu-library/ciment")
require(raster)

# require(sp, lib = "/home/dgeorges/R/x86_64-pc-linux-gnu-library/3.1/")
# require(raster, lib = "/home/dgeorges/R/x86_64-pc-linux-gnu-library/3.1/")

ras.tmpdir <- "/nfs_scratch2/dgeorges/R_raster_georges"
dir.create(ras.tmpdir, recursive = TRUE, showWarnings = FALSE)
rasterOptions(tmpdir = ras.tmpdir, ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive



## define the paths to the data and out dir ------------------------------------
in.ref <- "/nfs_scratch2/dgeorges/BRISCA/DATA/no_interaction_mask.grd"
in.occ <- "/nfs_scratch2/dgeorges/BRISCA/DATA/Occurrence.tables.combined.all.sources.no.outliers.merged"
out.dir <- "/nfs_scratch2/dgeorges/BRISCA/DATA/InvDistanceRaster/pts_files"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## define the output table
out.tab <- NULL #data.frame()

## load a reference raster
## define the projection system
proj <- CRS("+proj=laea +lat_0=90.0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") 
ref.ras <- raster(in.ref, crs = proj)

for(cell.id in cell.ids[1:2]){
  cat("\n\n cell", which(is.element(cell.ids, cell.id)))
  ## create the distance layers -----------------------------------------------
  ## build the distance raster for this cell
  cell.xy <- xyFromCell(object = ref.ras, cell = cell.id)
  cell.inv.dist.ras <- 1 / distanceFromPoints(object = ref.ras, xy = cell.xy)
  
  ## define a tmp output table
  out.tab.tmp <- data.frame(job.id = job.id, cell.id  = cell.id, cell.x = cell.xy[, "x"], cell.y = cell.xy[, "y"])
  
  for(sp.id in 1:nrow(param.tab)){
    cat("\t", sp.id)
  #   ## TEST
  #   sp.id <- 5
    sp.name <- param.tab$Genus.species[sp.id]
    sp.bmname <- param.tab$Biomod.name[sp.id]
    ## laod species occurences
    occ.dat <- read.table(file.path(in.occ, paste0(sp.name, ".txt")),
                          sep = "\t", header = TRUE, stringsAsFactors = FALSE)
    ## extract inverse distance values of occupied cells
    occ.inv.dist.df <- extract(x = cell.inv.dist.ras, y = occ.dat[, c('X', 'Y')], cellnumbers = TRUE)
    ## remove duplicated cells and the considered cell
    occ.inv.dist.df <- occ.inv.dist.df[!(duplicated(occ.inv.dist.df[, "cells"]) | occ.inv.dist.df[, "cells"] == cell.id), ]
    ## compute the summaries
    out.tab.tmp[[sp.bmname]] <- sum(occ.inv.dist.df[, 2], na.rm = TRUE)
    rownames(out.tab.tmp) <- NULL
  }
  out.tab <- rbind(out.tab, out.tab.tmp)
}

## save the file on the hard drive
write.table(out.tab, file = file.path(out.dir, paste0("inv_dist_", job.id, ".txt")), 
            quote = FALSE, append = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE)

quit("no")
## end of script -------------------------------------------------------------

# # Create params for grid --------------------------------------------------
# 
# job.pix.id.table <- read.table("~/BRISCA/WORKDIR/job_pix_id_table.txt", h = TRUE)
# param <- data.frame(job.id = unique(job.pix.id.table$job.id))
# 
# write.table(param, file = "~/BRISCA/WORKDIR/params_csoidra.txt", sep = " ", 
#             quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)

