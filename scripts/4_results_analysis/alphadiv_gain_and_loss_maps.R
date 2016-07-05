##' ---
##' @title Construct the alpha diversity, gain and loss maps in order to compute
##'   turnover maps
##' @descrtiption We will here construct 3 maps by scenario. The maps are based on SRC
##'   maps constructed with the common baseline.
##' @author damien georges
##' @date 2016-05-11
##' @licence GPL-2
##' ---

##' ## Initialisation

rm(list = ls())

## load needed libraries
library(raster)
library(dplyr)
library(multidplyr)
library(parallel)
library(ggplot2)
library(tidyr)

## set some parameters
same.baseline <- TRUE ## do we consider the same baseline (climate filtered no dispersal) as a baseline or 
                      ## each scenario current prediction as baseline
machine <- "sdiv" ## the name of the machine the script will run on
n.cores <- 16 ## number of resuired cores

## define the main paths to data
if(machine == "leca97"){
} else if (machine == "pinea"){
  briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on pinea
  src.maps.path <- paste0("~/Work/BRISCA/workdir/_SRC/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) ## on pinea
  param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt" ## on pinea
  out.dir.path <-"~/Work/BRISCA/outputs/2016-07-01" ## on pinea
} else if (machine == "sdiv"){
  briscahub.dir <- "~/BRISCA/briscahub/" ## on pinea
  src.maps.path <- paste0("/work/georges/BRISCA/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) ## on pinea
  param.tab.path <- "/work/georges/BRISCA/grid_params/params_src.txt" ## on pinea
  out.dir.path <-"/work/georges/BRISCA/outputs/2016-06-13" ## on pinea
  rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
                tmptime = 24, ## time after which raster tmp files will be deleted
                overwrite = TRUE)
} else stop("\n> unknow machine!")

dir.create(out.dir.path, showWarnings = FALSE, recursive =TRUE)


## load species ref table
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]

## load grid campain parameters table
param.tab <- read.table(param.tab.path, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
colnames(param.tab) <- c("mod.dir", "proj.dir", "file.pattern", "rcp", "gcm", "species")
## add the file id column
param.tab$file.id <- 1:nrow(param.tab)


## 1. get the filenames of the SRC maps for each scenario that interest us
src.maps.files <- list.files(src.maps.path, ".grd$", full.names = TRUE)

## check if some maps are missing
computed.jobs <- as.numeric(sub("_.*$", "", sub("src_baseline_", "", basename(src.maps.files))))
missing.jobs <- setdiff(param.tab$file.id, computed.jobs)
param.tab[missing.jobs, ]

## what we see here is that most of missing files are the one where we tried to
## filter the projections using convexhull. => because we decided not
## to consider this scenario anymore this is not a big deal!

param.tab[is.element(param.tab$file.id, missing.jobs) & !grepl("_filt_ch.grd$", param.tab$file.pattern), ]
## at the end only 4 jobs have failed! Let's lunch them again! => DONE

param.tab <- param.tab %>% group_by(file.id) %>%
  mutate(fut.file = paste0(mod.dir, "/", species, "/", proj.dir, "/individual_projections/", species, file.pattern), 
         model =  sub("_.*$", "", sub(paste0(species, "_"), "", basename(fut.file))), 
         scenario.full = sub(paste0(".*", species, "/"), "", dirname(dirname((fut.file)))),
         scenario.clim = sub(".*RCP_", "RCP_", scenario.full),
         scenario.biomod = basename(sub(paste0("/", species, ".*"), "", fut.file))
  ) %>% ungroup


## keep only the jobs that are interesting for us
gg.dat <- param.tab %>%  
  mutate(rcp = sub("_2080.*$", "", scenario.clim),
         gcm = sub("_(no|max)_disp.*$", "", sub(".*_2080_", "", scenario.clim)),
         biotic.inter = sub(paste0("^.*(", paste(unique(gcm), collapse="|"), ")"), "", scenario.clim),
         dispersal.filter = sub("^.*TSSbin", "", tools::file_path_sans_ext(file.pattern)),
         scenario.biomod = sub("_final", "", sub("Biomod_", "", scenario.biomod)))
## change dispersal filter labels
gg.dat$dispersal.filter[gg.dat$dispersal.filter == ""] <- "unlimited"
gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_ch"] <- "convex_hull"
gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_no_disp_invdist"] <- "no"
gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_min_disp_invdist"] <- "minimal"
gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_max_disp_invdist"] <- "maximal"
gg.dat <- gg.dat %>% filter(is.element(dispersal.filter, c("minimal", "maximal", "unlimited")))
## change biointeraction labels
gg.dat$biotic.inter[gg.dat$biotic.inter == ""] <- "no"
gg.dat$biotic.inter[gg.dat$biotic.inter == "_no_disp_invdist"] <- "low"
gg.dat$biotic.inter[gg.dat$biotic.inter == "_max_disp_invdist"] <- "high"
## change levels order
gg.dat$biotic.inter <- factor(gg.dat$biotic.inter, levels =  c("no", "low", "high"))
gg.dat$scenario.biomod <- factor(gg.dat$scenario.biomod, levels = c("pure_climate", "climate_and_biointer", "pure_climate_filtered", "climate_and_biointer_filtered"))
gg.dat$dispersal.filter <- factor(gg.dat$dispersal.filter, levels =  c("minimal", "maximal", "unlimited"))
## remove some combination of params we are not interested in
gg.dat <- gg.dat %>% filter(!(scenario.biomod == "climate_and_biointer_filtered" &  dispersal.filter == "no"),
                            !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "low" & dispersal.filter == "maximal"),
                            !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "high" & dispersal.filter == "minimal"))


## 2. compute for each scenario the nb of species lost/gain and the species richness by pixel

## check that no data is missing
gg.dat %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp) %>%
  summarize(nb.species = n()) %>% select(nb.species)

## define a function that calculates the alpha, SG, SL and turnover maps
calculate_alpha_gain_loss_turnover <- function(tab_){
  cat("\n ***")
  library(raster)
  library(dplyr)
  cat("\n> libraries loaded")
  src.maps.files_ <- src.maps.files[is.element(as.numeric(sub("_.*$", "", sub("src_baseline_", "", basename(src.maps.files)))), tab_$file.id)] 
  cat("\n> src.maps.file gotten (", length(src.maps.files_), ")")
  src.maps_ <- lapply(src.maps.files_, function(f_ ) raster(f_, RAT = FALSE))
  cat("\n> src.maps loaded")
  ## src.maps are codded like:
  #   -2 if the given pixel is predicted to be lost by the species. 
  #   -1 if the given pixel is predicted to be stable for the species.
  #   0 is the given pixel was not occupied, and will not be in the future.
  #   1 if the given pixel was not occupied, and is predicted to be into the future.
  
  occ.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 0,
                                                               -1.5, -0.5, 1,
                                                               -0.5, 0.5,  0,
                                                               0.5, 1.5, 1)))
  cat("\n> occ.maps porduced")
  alphadiv.map_ <- sum(raster::stack(occ.maps_))
  cat("\n> alphadiv.map porduced")
  
  gain.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 0,
                                                               -1.5, -0.5, 0,
                                                               -0.5, 0.5,  1,
                                                               0.5, 1.5, 0)))
  gain.map_ <- sum(raster::stack(gain.maps_))
  cat("\n> gain.map produced")
  
  lost.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 1,
                                                                -1.5, -0.5, 0,
                                                                -0.5, 0.5,  0,
                                                                0.5, 1.5, 0)))
  lost.map_ <- sum(raster::stack(lost.maps_))
  cat("\n> lost.map produced")
  
  turnover.map_ <- (lost.map_ + gain.map_) / (gain.map_ + alphadiv.map_)
  cat("\n> turnover.map produced")
  
  out.stack_ <- raster::brick(alphadiv.map_, 
                              gain.map_, 
                              lost.map_, 
                              turnover.map_)
  names(out.stack_) <- c("alphadiv", "gain", "lost", "turnover")
  
  ## save the output stack on hard drive
  stack.file.name_ <- file.path(out.dir.path, paste0("summaryStack__", 
                                                    paste0(unique(tab_$scenario.biomod, collapse = "-")), "__",
                                                    paste0(unique(tab_$biotic.inter, collapse = "-")), "__",
                                                    paste0(unique(tab_$dispersal.filter, collapse = "-")), "__",
                                                    paste0(unique(tab_$gcm, collapse = "-")), "__",
                                                    paste0(unique(tab_$rcp, collapse = "-")), ".grd"))
  cat("\n> out.stack saved as", stack.file.name_)
  writeRaster(out.stack_, filename = stack.file.name_, overwrite = TRUE)
  
  cat("\n ***")
  return(stack.file.name_)
}

### test
## sequential version
tab_ <- gg.dat %>% 
  filter(scenario.biomod == scenario.biomod[1],
         biotic.inter == biotic.inter[1],
         dispersal.filter == dispersal.filter[1],
         gcm == gcm[1],
         rcp == rcp[1]) #%>%
#   group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp) %>%
#   do(stack.file.name = calculate_alpha_gain_loss_turnover(.))



### end of test 


if(n.cores <= 1){
  ## sequential version
  gg.calc <- gg.dat %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp) %>%
    do(stack.file.name = calculate_alpha_gain_loss_turnover(.))
} else{
  ## parallel version
  clust <- create_cluster(cores = n.cores, quiet = FALSE)
  clusterExport(clust,c("calculate_alpha_gain_loss_turnover", "out.dir.path", "src.maps.files"))
  gg.dat.part <- partition(gg.dat, scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, cluster = clust)
  gg.calc <- gg.dat.part %>% do(stack.file.name = calculate_alpha_gain_loss_turnover(.))
  stopCluster(clust)
}



save(gg.calc, file = file.path(out.dir.path, "gg.calc.RData"))


##' @note
##'  - has to be designed in parallel way with multidplyr package
##'  - check why raster stacks are not well saved (bands insted of layers)
##'  - add the path to tmp raster dir to prevent from filling /tmp of the cluster



# library(rasterVis)
# r1 <- raster("~/SRC_maps/src_7843_001.grd", RAT = TRUE)
# r2 <- raster("~/SRC_maps/src_7844_001.grd")
# 
# r1 <- ratify(r1* r_b)
# r2 <- ratify(r2* r_b)
# 
# rat <- levels(r1)[[1]]
# rat$ID <- c(0, -2, 1, -1)
# rat$status <- factor(c('Abs', 'Lost', 'Gain', 'Pres'), levels = c('Abs', 'Lost', 'Gain', 'Pres'))
# levels(r1) <- levels(r2) <- rat
# s <- stack(r2,r1) 
# names(s) <- c("no dispersal", "max dispersal")
# 
# myTheme <- rasterTheme(region=c('#c2a5cf','#7b3294','#008837','#a6dba0'))
# levelplot(s,  par.settings = myTheme, main = "SRC for Cassiope tetragona - climat + biotic + dispersal") + 
#   layer(sp.lines(s_sa, lwd=0.8, col='black')) +
#   layer(sp.lines(s_la, lwd=0.8, lty = 2, col='black')) +
#   layer(sp.lines(s_ha, lwd=0.8, lty = 3, col='black')) 
# # +
# #   layer({
# #     SpatialPolygonsRescale(layout.north.arrow(),
# #                            offset = c(-4e+06,-4e+06),
# #                            scale = 1000000)
# #   })
# 
# 
# ## add the 
# library(grid)
# # grid.ls(viewport=TRUE, grobs=FALSE)  ## Prints out a listing of all viewports in plot
# # grid.rect(vp = "plot_01.toplevel.vp::plot_01.legend.right.vp",
# #           gp = gpar(col = "red"))
# ll <- seekViewport("plot_01.legend.right.vp")
# grid.text(c("___ sub-arctic\n_ _  low-arctic\n..... high-arctic"), x = 0, y = unit(1, "lines"), 
#           just = c("left", "bottom"),
#           gp = gpar(cex=0.8))
# upViewport(ll)  
