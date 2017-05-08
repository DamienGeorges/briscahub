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

.libPaths("/home/georges/R/x86_64-pc-linux-gnu-library/3.2/")
library(raster)
# require(pROC)
require(biomod2, lib.loc = "/home/georges/R/biomod2_pkg/biomod2_3.1-73-04")
require(rgdal)
library(dplyr)

rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              #               chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              #               maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE)

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
file.id <- as.numeric(args[1])
## file.id <- 7646

output.tab.dir <- "/work/georges/BRISCA/SRC_baseline_tabs_2017-05-08"
output.map.dir <- "/work/georges/BRISCA/SRC_baseline_maps_2017-05-08"
path.to.buffers <- "/home/georges/BRISCA/briscahub/data/mask_raster_arctic_area_2017-04-26"
param.file <- "/work/georges/BRISCA/grid_params/params_src_2017-05-08.txt"
briscahub.dir <- "/home/georges/BRISCA/briscahub"

dir.create(output.tab.dir, recursive=TRUE, showWarnings=FALSE)
dir.create(output.map.dir, recursive=TRUE, showWarnings=FALSE)

# ## -- load th species list -----------------------------------------------------
# sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
#                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
# sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]
# 
# cat("\n> sp.tab\n")
# head(sp.tab)

##' -- read the new ref grid ---------------------------------------------------

## load couple of masks to compute stats locally
r.full.area <- raster(file.path(path.to.buffers, "mask_full_area_no_ice.grd"))
r.from.sa <- raster(file.path(path.to.buffers, "mask_from_subarctic_area_no_ice.grd"))
r.sa <- raster(file.path(path.to.buffers, "mask_subarctic_area_no_ice.grd"))
r.from.la <- raster(file.path(path.to.buffers, "mask_from_lowarctic_area_no_ice.grd"))
r.la <- raster(file.path(path.to.buffers, "mask_lowarctic_area_no_ice.grd"))
r.ha <- raster(file.path(path.to.buffers, "mask_higharctic_area_no_ice.grd"))

mask.ids <- c('r.full.area', 'r.from.sa', 'r.sa', 'r.from.la', 'r.la', 'r.ha')

##' load parameters ------------------------------------------------------------
param.tab <- read.table(param.file, sep = "\t", header = TRUE, stringsAsFactors=FALSE)

sp <- param.tab$sp[file.id]
model <- param.tab$model[file.id]
rcp <- param.tab$rcp[file.id]
gcm <- param.tab$gcm[file.id]
filt <- param.tab$filt[file.id]
biointer <- param.tab$biointer[file.id]
biointer.intenisty <- param.tab$biointer.intenisty[file.id]

cat("\n--job parameters --------")
cat("\n> file.id:", file.id, "\n")
cat("\n> sp:", sp, "\n")
cat("\n> model:", model, "\n")
cat("\n> rcp:", rcp, "\n")
cat("\n> gcm:", gcm, "\n")
cat("\n> filt:", filt, "\n")
cat("\n> biointer:", biointer, "\n")
cat("\n> biointer.intenisty", biointer.intenisty, "\n")
cat("\n--------------------------")

##' the path to the models/projection files -------------------------------------
pure.clim.dir <- "/work/georges/BRISCA/Biomod_pure_climate_2017_03_09"
climate.and.biointer.incltree.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_incl_tree_2017-04-07"
climate.and.biointer.notree.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_no_tree_2017-04-07"
pres.day.mask.dir <- "/work/georges/BRISCA/Present_day_masks_2017_03_17"
fut.day.mask.dir <- "/work/georges/BRISCA/Future_day_masks_2017_03_17"


## the baslline is the current projection filtered by the no dispersal filter (currently Convex Hull + 250km buffer)
r.baseline <- raster(file.path(pure.clim.dir, sp, "proj_pure_climat_current", "individual_projections", paste0(sp, model)))
r.pres.day.mask <- raster(file.path(pres.day.mask.dir, paste0(sp, "_present_day_mask.grd")))
r.baseline <- r.baseline * r.pres.day.mask * r.full.area

fut.day.mask <- raster(file.path(fut.day.mask.dir, "max_dispersal", paste0(sp, "_future_day_max_disp_mask.grd")))

## the future raster we want to compute SRC with
mod.dir <- switch(biointer,
                  no = pure.clim.dir,
                  no_tree = climate.and.biointer.notree.dir,
                  incl_tree = climate.and.biointer.incltree.dir)

r.filt.mask <- switch(filt,
                 unlimited_dipersal = r.full.area,
                 no_dipersal = r.pres.day.mask,
                 max_dipersal = fut.day.mask)
  
r.fut <- raster(file.path(mod.dir, sp, paste0("proj_pure_climat_", rcp, "_2080_", gcm, ifelse(biointer != "no", biointer.intenisty, "")), 
                   "individual_projections", paste0(sp, model)) )

r.fut <- r.fut * r.filt.mask

## COMPUTE THE SRC 
cat("\n src within full area")
sp.rc.full <- biomod2::BIOMOD_RangeSize(CurrentPred = r.baseline ,FutureProj = r.fut)
sp.rc.full.tab <- as.data.frame(sp.rc.full$Compt.By.Models)
sp.rc.full.tab$area <- "full_area"

cat("\n src from sub arctic")
sp.rc.fsa <- biomod2::BIOMOD_RangeSize(CurrentPred = r.baseline * r.from.sa, FutureProj = r.fut * r.from.sa)
sp.rc.fsa.tab <- as.data.frame(sp.rc.fsa$Compt.By.Models)
sp.rc.fsa.tab$area <- "from_sub_arctic"

cat("\n src within sub arctic")
sp.rc.sa <- biomod2::BIOMOD_RangeSize(CurrentPred = r.baseline * r.sa, FutureProj = r.fut * r.sa)
sp.rc.sa.tab <- as.data.frame(sp.rc.sa$Compt.By.Models)
sp.rc.sa.tab$area <- "sub_arctic"

cat("\n src from low arctic")
sp.rc.fla <- biomod2::BIOMOD_RangeSize(CurrentPred = r.baseline * r.from.la, FutureProj = r.fut * r.from.la)
sp.rc.fla.tab <- as.data.frame(sp.rc.fla$Compt.By.Models)
sp.rc.fla.tab$area <- "from_low_arctic"

cat("\n src within low arctic")
sp.rc.la <- biomod2::BIOMOD_RangeSize(CurrentPred = r.baseline * r.la, FutureProj = r.fut * r.la)
sp.rc.la.tab <- as.data.frame(sp.rc.la$Compt.By.Models)
sp.rc.la.tab$area <- "low_arctic"

cat("\n src within high arctic")
sp.rc.ha <- biomod2::BIOMOD_RangeSize(CurrentPred = r.baseline * r.ha, FutureProj = r.fut * r.ha)
sp.rc.ha.tab <- as.data.frame(sp.rc.ha$Compt.By.Models)
sp.rc.ha.tab$area <- "high_arctic"

## define the raster output directory
src_tab_file <- file.path(output.tab.dir, paste0("src_baseline_", sprintf("%05d", file.id), ".txt"))
src_ras_file <- file.path(output.map.dir, paste0("src_baseline_", sprintf("%05d", file.id), ".grd"))


sp.rc.tab <- rbind(sp.rc.full.tab, sp.rc.fsa.tab, sp.rc.sa.tab, sp.rc.fla.tab, sp.rc.la.tab, sp.rc.ha.tab)
sp.rc.tab$file.id <- file.id
sp.rc.tab$sp <- sp
sp.rc.tab$model <- model
sp.rc.tab$rcp <- rcp
sp.rc.tab$gcm <- gcm
sp.rc.tab$filt <- filt
sp.rc.tab$biointer <- biointer
sp.rc.tab$biointer.intenisty <- biointer.intenisty
sp.rc.tab$src_ras_file <- src_ras_file

write.table(sp.rc.tab, 
            file = src_tab_file,
            sep = "\t", row.names = TRUE, col.names = FALSE)

writeRaster(sp.rc.full$Diff.By.Pixel, 
            filename = src_ras_file,
            datatype = "INT1S",
	          NAflag = -127,
            overwrite = TRUE)

cat("\n> Succeed!!!")

q("no")
###################################################################################################

library(dplyr)
out.dir <- "/work/georges/BRISCA/grid_params/"
sp.tab <- read.table("~/BRISCA/briscahub/data/sp.list_03.03.2017.txt", header = TRUE, 
                     sep = "\t", stringsAsFactors = FALSE)
# sp.tab <- read.table("~/Work/BRISCA/briscahub/data/sp.list_03.03.2017.txt", header = TRUE, 
#                      sep = "\t", stringsAsFactors = FALSE)

sp.tab <- sp.tab %>% filter(Growth.form.height == 'SHRUB')

sp_ <- sp.tab$Biomod.name
models_ <- "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd"
rcp_ <- c("RCP_2.6", "RCP_4.5", "RCP_6.0", "RCP_8.5")
gcm_ <- c("cesm1_cam5", "csiro_mk360", "gfdl_esm2m", "miroc_miroc5", "mri_cgcm3", "nimr_hadgem2ao")
filt_ <- c("unlimited_dipersal","no_dipersal", "max_dipersal")
biointer_type_ <- c("no", "no_tree", "incl_tree")
biointer_intensity_ <- c("no", "unlimited_dipersal","no_dipersal", "max_dipersal")


params <- expand.grid(sp = sp_, model = models_, rcp = rcp_, gcm = gcm_, filt = filt_, biointer = biointer_type_, biointer.intenisty = biointer_intensity_)
params <- params %>% left_join(sp.tab %>% dplyr::select(Biomod.name, Growth.form.Isla) %>% rename(sp = Biomod.name, gf = Growth.form.Isla)) 
## remove the unrealistic combination
params  <- params %>% 
  filter(!(gf %in% c("Tree", "Tall shrub") & biointer == "no_tree"),
         !(biointer == "no" & biointer.intenisty %in% c("unlimited_dipersal","no_dipersal", "max_dipersal")),
         !(biointer %in% c("no_tree", "incl_tree") & biointer.intenisty == "no"))

write.table(params, file = file.path(out.dir, "params_src_2017-05-08.txt"), sep = "\t", col.names = T)

############################################################################
## merge results
output.tab.dir <- "/work/georges/BRISCA/SRC_baseline_tabs_2017-05-08"
sp.tab <- read.table("~/BRISCA/briscahub/data/sp.list_03.03.2017.txt", header = TRUE, 
                     sep = "\t", stringsAsFactors = FALSE)
l.files <- list.files(output.tab.dir, full.names = TRUE)
tab.out <- lapply(l.files, read.table, header = FALSE, sep = "\t", stringsAsFactors = FALSE, row.names = NULL)
tab.out <- bind_rows(tab.out)
colnames(tab.out) <- c("layer_id", "Loss", "Stable0", "Stable1", "Gain", "PercLoss",
                       "PercGain", "SpeciesRangeChange", "CurrentRangeSize", 
                       "FutureRangeSize.NoDisp", "FutureRangeSize.FullDisp",
                       "area", "file.id", "sp", "model", "rcp", "gcm", "filt", 
                       "biointer", "biointer_intensity","src_ras_file")
tab.out <- tab.out %>% left_join(sp.tab %>% dplyr::select(Biomod.name, Growth.form.Isla) %>% rename(sp = Biomod.name, gf = Growth.form.Isla)) 
## duplicate the with_tree biointer results for the no_tree ones  
tab.out.temp <- tab.out %>% filter(biointer == "incl_tree" & gf %in% c("Tall shrub")) %>% mutate(biointer = "no_tree")
tab.out <- tab.out %>% bind_rows(tab.out.temp) %>% distinct

write.table(tab.out, file = paste0(output.tab.dir, ".txt"), sep = "\t", col.names = T)