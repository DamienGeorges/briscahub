##' ------------------------------------------------
##' @title spicies range change somputation
##' @description In this script we will produce 
##'   species range change of all species based on
##'   scenario, models, ...
##' @author damien g.
##' @licence GPL-2
##' ------------------------------------------------

.libPaths("/gpfs0/home/georges/R/x86_64-pc-linux-gnu-library/3.2/")
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

output.tab.dir <- "/work/georges/BRISCA/SRC_baseline_tabs_new"
output.map.dir <- "/work/georges/BRISCA/SRC_baseline_maps_new"
path.to.buffers <- "/home/georges/BRISCA/briscahub/data/mask_raster_arctic_area_2016-08-22"
briscahub.dir <- "/home/georges/BRISCA/briscahub"
param.file <- file.path(briscahub.dir, "data/params_src_new.RData")

dir.create(output.tab.dir, recursive=TRUE, showWarnings=FALSE)
dir.create(output.map.dir, recursive=TRUE, showWarnings=FALSE)

## -- load th species list -----------------------------------------------------
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]

cat("\n> sp.tab\n")
#head(sp.tab)

##' -- read the new ref grid ---------------------------------------------------
# ref.ras.buffer <- raster(file.path(path.to.buffers, "Buffer.grd"))
# ref.ras.from.sa <- raster(file.path(path.to.buffers, "Sub_Arctic.grd"))
# ref.ras.from.la <- raster(file.path(path.to.buffers, "Low_Arctic.grd"))
# ref.ras.from.ha <- raster(file.path(path.to.buffers, "High_Arctic.grd"))
## load couple of masks to compute stats locally
r.full.area <- raster(file.path(path.to.buffers, "mask_full_area_no_ice.grd"))
r.from.sa <- raster(file.path(path.to.buffers, "mask_from_subarctic_area_no_ice.grd"))
r.sa <- raster(file.path(path.to.buffers, "mask_subarctic_area_no_ice.grd"))
r.from.la <- raster(file.path(path.to.buffers, "mask_from_lowarctic_area_no_ice.grd"))
r.la <- raster(file.path(path.to.buffers, "mask_lowarctic_area_no_ice.grd"))
r.ha <- raster(file.path(path.to.buffers, "mask_higharctic_area_no_ice.grd"))

mask.ids <- c('r.full.area', 'r.from.sa', 'r.sa', 'r.from.la', 'r.la', 'r.ha')



# ref.poly@data$WATER <- factor(ref.poly@data$WATER, levels = c(0,1,2), labels = c("water", "arctic", "sub-arctic") )

# param.list <- read.table(param.file, sep = "\t", header = FALSE, stringsAsFactors=FALSE)
param.list <- get(load(param.file))

cat("\n> param.list\n")
head(param.list)

# src.stk <- NULL
# src.stk.list <- lapply(sp.tab$Biomod.name, function(sp_){
# for(sp_ in sp.tab$Biomod.name){
sp_ <- param.list[file.id, 6]
cat("\n> sp_:", sp_,"\n")

cat("\n> file.id:", file.id, "\n")

species <- param.list$species[file.id]
model <-  param.list$model[file.id]
scenario.full <- param.list$scenario.full[file.id]
scenario.clim <- param.list$scenario.clim[file.id]
scenario.biomod <- param.list$scenario.biomod[file.id]

fut.file <- paste0(param.list[file.id, 1], "/", sp_, "/", param.list[file.id, 2], "/individual_projections/", sp_, param.list[file.id, 3]) 
# fut.file <- "/work/georges/BRISCA/Biomod_pure_climate_final/Abies.balsamea/proj_pure_climat_RCP_2.6_2080_cesm1_cam5/individual_projections/Abies.balsamea_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"
# 

## not the same baseline
# cur.file <- sub("RCP_.*/individual_projections", "current/individual_projections", fut.file)
# if(!file.exists(cur.file)){
#   if(grepl("_no_disp_invdist", fut.file)) cur.file <- sub("current", "current_no_disp_invdist", cur.file)
#   if(grepl("_max_disp_invdist", fut.file)) cur.file <- sub("current", "current_max_disp_invdist", cur.file)
# }

## the same baseline
## load the projection baseline
cur.file <- paste0("/data/idiv_sdiv/brisca/results/Biomod_pure_climate_filtered/", species, "/proj_pure_climat_current/individual_projections/", species, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin_filt_no_disp_invdist.grd") 

## load the species rasters
r.cur <- raster(cur.file)
r.fut <- raster(fut.file)

## resahpe the rasters
## Fucking trick to cheet with R GIS projection coordinates issue (e.g make a 180° rotatin)
proj.ref1 <- "+proj=laea +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj.ref2 <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

r.cur <- projectRaster(r.cur, crs = CRS(proj.ref2))
r.fut <- projectRaster(r.fut, crs = CRS(proj.ref2))
projection(r.cur) <- proj.ref1
projection(r.fut) <- proj.ref1


r.cur[is.na(r.cur[])] <- 0
r.fut[is.na(r.fut[])] <- 0
r.cur <- crop(r.cur, r.full.area) * r.full.area
r.fut <- crop(r.fut, r.full.area) * r.full.area

cat("\n src on full area")
sp.rc <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur ,FutureProj = r.fut)
sp.rc.tab <- as.data.frame(sp.rc$Compt.By.Models)
sp.rc.tab$area <- "full"


cat("\n src on sub arctic")
sp.rc.from.sa <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * r.from.sa, 
                                      FutureProj = r.fut * r.from.sa)
sp.rc.from.sa.tab <- as.data.frame(sp.rc.from.sa$Compt.By.Models)
sp.rc.from.sa.tab$area <- "from_sub_arctic"

sp.rc.sa <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * r.sa, 
                                      FutureProj = r.fut * r.sa)
sp.rc.sa.tab <- as.data.frame(sp.rc.sa$Compt.By.Models)
sp.rc.sa.tab$area <- "sub_arctic"


cat("\n src on low arctic")
sp.rc.from.la <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * r.from.la, 
                                           FutureProj = r.fut * r.from.la)
sp.rc.from.la.tab <- as.data.frame(sp.rc.from.la$Compt.By.Models)
sp.rc.from.la.tab$area <- "from_low_arctic"

sp.rc.la <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * r.la, 
                                      FutureProj = r.fut * r.la)
sp.rc.la.tab <- as.data.frame(sp.rc.la$Compt.By.Models)
sp.rc.la.tab$area <- "low_arctic"


cat("\n src on high arctic")
sp.rc.ha <- biomod2::BIOMOD_RangeSize(CurrentPred = r.cur * r.ha, 
                                      FutureProj = r.fut * r.ha)
sp.rc.ha.tab <- as.data.frame(sp.rc.ha$Compt.By.Models)
sp.rc.ha.tab$area <- "high_arctic"


sp.rc.tab <- rbind(sp.rc.tab, sp.rc.sa.tab, sp.rc.la.tab, sp.rc.ha.tab,
                   sp.rc.from.sa.tab, sp.rc.from.la.tab)
sp.rc.tab <- cbind(param.list[file.id,,drop=FALSE], sp.rc.tab)

# sp.rc.tab$species <- species
# sp.rc.tab$model <- model
# sp.rc.tab$scenario.clim <- scenario.clim
# sp.rc.tab$scenario.biomod <- scenario.biomod
# sp.rc.tab$file.id <- file.id
# sp.rc.tab$sp.id <- which(sp.tab$Biomod.name %in% sp_)


write.table(sp.rc.tab, 
            file=file.path(output.tab.dir, paste0("src_", sprintf("%05d", file.id), "_", sprintf("%03d", unique(sp.rc.tab$sp.id)), ".txt")),
            sep = "\t", row.names = FALSE, col.names = FALSE)

writeRaster(sp.rc$Diff.By.Pixel * r.full.area, filename = file.path(output.map.dir, paste0("src_", sprintf("%05d", file.id), "_", sprintf("%03d", unique(sp.rc.tab$sp.id)), ".grd")),
            datatype = "INT1S",
	    NAflag = -127,
            overwrite = TRUE)

q("no")

##' parameters creation

### SEE create_new_src_params.R ###






# 
# xx <- readLines("/work/georges/BRISCA/grid_params/params_clip_output_maps.txt")
# xx <- grep(".grd$", xx, value=TRUE) ## keep only grd files
# xx <- grep("_TSSbin", xx, value=TRUE) ## keep only binaries
# xx <- grep("_EMcaByTSS", xx, value=TRUE) ## keep only models based on CA
# param.list.b <- data.frame(path.to.mod = sapply(xx, function(p_){paste0(unlist(strsplit(x=p_, split="/"))[1:5], collapse = "/")}),
#                            rcp_gcm = sapply(xx, function(p_){paste0(unlist(strsplit(x=p_, split="/"))[7], collapse = "/")}),
#                            file_pattern = sapply(xx, function(p_){sub(".*_EMcaByTSS", "_EMcaByTSS", unlist(strsplit(x=p_, split="/"))[9])}))
# param.list.b$merged_pattern <- paste(param.list.b$path.to.mod, param.list.b$rcp_gcm, param.list.b$file_pattern, sep = "_")
# param.list.c <- param.list.b[!duplicated(param.list.b$merged_pattern), ]
# rownames(param.list.c) <- NULL
# 
# param.list.d <- param.list.c[, 1:3]
# param.list.d$rcp <- paste0("RCP_", sub("_.*", "", sub(".*RCP_", "", param.list.d$rcp_gcm)))
# param.list.d$gcm <- sub(".*2080_", "", param.list.d$rcp_gcm)
# 
# ## remove the current files
# param.list.d <- param.list.d[!grepl("current", param.list.d$rcp_gcm),]
# 
# ## add the species line
# sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
#                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
# sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]
# param.list.e <- expand.grid.df(param.list.d, data.frame(sp.bmn = sp.tab$Biomod.name))
# 
# 
# write.table(param.list.e, file="/work/georges/BRISCA/grid_params/params_src.txt", sep="\t", row.names=FALSE, col.names=FALSE)
# 
