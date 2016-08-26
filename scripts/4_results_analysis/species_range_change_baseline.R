##' ------------------------------------------------
##' @title spicies range change somputation
##' @description In this script we will produce 
##'   species range change of all species based on
##'   scenario, models, ...
##'   The main difference with the first run of SRC is that we will
##'   always consider the projections of current climate + no dispersal
##'   as baseline of the SRC
##' @author damien g.
##' @licence GPL-2
##' ------------------------------------------------

library(raster)
require(pROC, lib.loc = "/gpfs0/home/georges/R/x86_64-pc-linux-gnu-library/3.2/")
require(rasterVis, lib.loc = "/gpfs0/home/georges/R/x86_64-pc-linux-gnu-library/3.2/")
require(biomod2, lib.loc = "/home/georges/R/biomod2_pkg/biomod2_3.1-73-04")

rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              #               chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              #               maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE)

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
file.id <- as.numeric(args[1])
## file.id <- 7646

output.tab.dir <- "/work/georges/BRISCA/SRC_baseline_tabs"
output.map.dir <- "/work/georges/BRISCA/SRC_baseline_maps"
path.to.buffers <- "/home/georges/BRISCA/briscahub/data/Arctic_buffers"
param.file <- "/work/georges/BRISCA/grid_params/params_src.txt"
briscahub.dir <- "/home/georges/BRISCA/briscahub"

dir.create(output.tab.dir, recursive=TRUE, showWarnings=FALSE)
dir.create(output.map.dir, recursive=TRUE, showWarnings=FALSE)

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]

cat("\n> sp.tab\n")
head(sp.tab)

##' -- read the new ref grid ---------------------------------------------------
ref.ras.buffer <- raster(file.path(path.to.buffers, "Buffer.grd"))
ref.ras.from.sa <- raster(file.path(path.to.buffers, "Sub_Arctic.grd"))
ref.ras.from.la <- raster(file.path(path.to.buffers, "Low_Arctic.grd"))
ref.ras.from.ha <- raster(file.path(path.to.buffers, "High_Arctic.grd"))

# ref.poly@data$WATER <- factor(ref.poly@data$WATER, levels = c(0,1,2), labels = c("water", "arctic", "sub-arctic") )

param.list <- read.table(param.file, sep = "\t", header = FALSE, stringsAsFactors=FALSE)

cat("\n> param.list\n")
head(param.list)

# src.stk <- NULL
# src.stk.list <- lapply(sp.tab$Biomod.name, function(sp_){
# for(sp_ in sp.tab$Biomod.name){
sp_ <- param.list[file.id, 6]
cat("\n> sp_:", sp_,"\n")

cat("\n> file.id:", file.id, "\n")


fut.file <- paste0(param.list[file.id, 1], "/", sp_, "/", param.list[file.id, 2], "/individual_projections/", sp_, param.list[file.id, 3]) 
# fut.file <- "/work/georges/BRISCA/Biomod_pure_climate_final/Abies.balsamea/proj_pure_climat_RCP_2.6_2080_cesm1_cam5/individual_projections/Abies.balsamea_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"


species <- sub("_.*$", "", basename(fut.file))
model <-  sub("_.*$", "", sub(paste0(species, "_"), "", basename(fut.file)))
scenario.full <- sub(paste0(".*", species, "/"), "", dirname(dirname((fut.file))))
scenario.clim <- sub(".*RCP_", "RCP_", scenario.full)
scenario.biomod <- basename(sub(paste0("/", species, ".*"), "", fut.file))

## load the projection baseline
cur.file <- paste0("/work/georges/BRISCA/Biomod_pure_climate_filtered/", species, "/proj_pure_climat_current/individual_projections/", species, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin_filt_no_disp_invdist.grd") 

## load the species rasters
r.cur <- raster(cur.file)
r.fut <- raster(fut.file)

## resahpe the rasters
r.cur <- projectRaster(r.cur, ref.ras.buffer)
r.fut <- projectRaster(r.fut, ref.ras.buffer)

r.cur[is.na(r.cur[])] <- 0
r.fut[is.na(r.fut[])] <- 0
r.cur <- crop(r.cur, ref.ras.buffer) * ref.ras.buffer
r.fut <- crop(r.fut, ref.ras.buffer) * ref.ras.buffer

cat("\n src on full area")
sp.rc <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur ,FutureProj = r.fut)
sp.rc.tab <- as.data.frame(sp.rc$Compt.By.Models)
sp.rc.tab$area <- "full"

cat("\n src on sub arctic")
sp.rc.sa <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * ref.ras.from.sa, 
                                      FutureProj = r.fut * ref.ras.from.sa)
sp.rc.sa.tab <- as.data.frame(sp.rc.sa$Compt.By.Models)
sp.rc.sa.tab$area <- "from_sub_arctic"

cat("\n src on low arctic")
sp.rc.la <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * ref.ras.from.la, 
                                      FutureProj = r.fut * ref.ras.from.la)
sp.rc.la.tab <- as.data.frame(sp.rc.la$Compt.By.Models)
sp.rc.la.tab$area <- "from_low_arctic"

cat("\n src on high arctic")
sp.rc.ha <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * ref.ras.from.ha, 
                                      FutureProj = r.fut * ref.ras.from.ha)
sp.rc.ha.tab <- as.data.frame(sp.rc.ha$Compt.By.Models)
sp.rc.ha.tab$area <- "from_high_arctic"

sp.rc.tab <- rbind(sp.rc.tab, sp.rc.sa.tab, sp.rc.la.tab, sp.rc.ha.tab)
sp.rc.tab$species <- species
sp.rc.tab$model <- model
sp.rc.tab$scenario.clim <- scenario.clim
sp.rc.tab$scenario.biomod <- scenario.biomod
sp.rc.tab$file.id <- file.id
sp.rc.tab$sp.id <- which(sp.tab$Biomod.name %in% sp_)


write.table(sp.rc.tab, 
            file=file.path(output.tab.dir, paste0("src_baseline_", sprintf("%05d", file.id), "_", sprintf("%03d", unique(sp.rc.tab$sp.id)), ".txt")),
            sep = "\t", row.names = FALSE, col.names = FALSE)

writeRaster(sp.rc$Diff.By.Pixel * ref.ras.buffer, filename = file.path(output.map.dir, paste0("src_baseline_", sprintf("%05d", file.id), "_", sprintf("%03d", unique(sp.rc.tab$sp.id)), ".grd")),
            datatype = "INT1S",
	    NAflag = -127,
            overwrite = TRUE)

q("no")

##' parameters creation

xx <- readLines("/work/georges/BRISCA/grid_params/params_clip_output_maps.txt")
xx <- grep(".grd$", xx, value=TRUE) ## keep only grd files
xx <- grep("_TSSbin", xx, value=TRUE) ## keep only binaries
xx <- grep("_EMcaByTSS", xx, value=TRUE) ## keep only models based on CA
param.list.b <- data.frame(path.to.mod = sapply(xx, function(p_){paste0(unlist(strsplit(x=p_, split="/"))[1:5], collapse = "/")}),
                           rcp_gcm = sapply(xx, function(p_){paste0(unlist(strsplit(x=p_, split="/"))[7], collapse = "/")}),
                           file_pattern = sapply(xx, function(p_){sub(".*_EMcaByTSS", "_EMcaByTSS", unlist(strsplit(x=p_, split="/"))[9])}))
param.list.b$merged_pattern <- paste(param.list.b$path.to.mod, param.list.b$rcp_gcm, param.list.b$file_pattern, sep = "_")
param.list.c <- param.list.b[!duplicated(param.list.b$merged_pattern), ]
rownames(param.list.c) <- NULL

param.list.d <- param.list.c[, 1:3]
param.list.d$rcp <- paste0("RCP_", sub("_.*", "", sub(".*RCP_", "", param.list.d$rcp_gcm)))
param.list.d$gcm <- sub(".*2080_", "", param.list.d$rcp_gcm)

## remove the current files
param.list.d <- param.list.d[!grepl("current", param.list.d$rcp_gcm),]

## add the species line
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]
param.list.e <- expand.grid.df(param.list.d, data.frame(sp.bmn = sp.tab$Biomod.name))


write.table(param.list.e, file="/work/georges/BRISCA/grid_params/params_src.txt", sep="\t", row.names=FALSE, col.names=FALSE)


