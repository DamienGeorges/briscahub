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

cat("\n memory.limit() = ", memory.limit())

## set some parameters
machine <- "sdiv" # "sdiv" ## the name of the machine the script will run on
n.cores <- 1 ## number of resuired cores

## define the main paths to data
if(machine == "leca97"){
  briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on leca97
  src.maps.path <-  paste0("~/Work/BRISCA/workdir/_SRC/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) ## on leca97
  param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt"
  out.dir.path <- paste0("~/Work/BRISCA/outputs/2016-08-18/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack") ## on leca97
} else if (machine == "pinea"){
  briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on pinea
  src.maps.path <- paste0("~/Work/BRISCA/workdir/_SRC/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) ## on pinea
  param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt" ## on pinea
  out.dir.path <-"~/Work/BRISCA/outputs/2016-07-01" ## on pinea
} else if (machine == "sdiv"){
  briscahub.dir <- "~/BRISCA/briscahub/" ## on pinea
  src.maps.path <- "/work/georges/BRISCA/SRC_baseline_maps_2017-04-26"
  src.out.tab.file <- "/work/georges/BRISCA/SRC_baseline_tabs_2017-04-26.txt"
  param.tab.path <- "/work/georges/BRISCA/grid_params/params_alphadiv_2017-04-25.txt" ## on pinea
  out.dir.path <-"/work/georges/BRISCA/alphadiv_2017-04-25"
  path.to.buffers <- "/home/georges/BRISCA/briscahub/data/mask_raster_arctic_area_2017-04-26"
  
  rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
                tmptime = 24, ## time after which raster tmp files will be deleted
                overwrite = TRUE)
} else stop("\n> unknow machine!")

dir.create(out.dir.path, showWarnings = FALSE, recursive =TRUE)

args <- commandArgs(trailingOnly = TRUE)
job.id <- as.character(args[1]) ## job.id <-  1 


## load species ref table
## load grid campain parameters table
src.out.tab <- read.table(src.out.tab.file, 
                     sep = "\t", stringsAsFactors = FALSE, header = TRUE)
src.out.tab <- src.out.tab %>% dplyr::select(sp, filt, biointer, gf, src_ras_file, rcp, gcm) %>% distinct
  
param.tab <- read.table(param.tab.path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
param.tab <- param.tab[job.id,,drop = FALSE]
param.tab.job <- param.tab %>% left_join(src.out.tab)

## 1. get the filenames of the SRC maps for each scenario that interest us
src.maps.files <- param.tab.job$src_ras_file

## load couple of masks to compute stats locally
r.full.area <- raster(file.path(path.to.buffers, "mask_full_area_no_ice.grd"))
r.from.sa <- raster(file.path(path.to.buffers, "mask_from_subarctic_area_no_ice.grd"))
r.sa <- raster(file.path(path.to.buffers, "mask_subarctic_area_no_ice.grd"))
r.from.la <- raster(file.path(path.to.buffers, "mask_from_lowarctic_area_no_ice.grd"))
r.la <- raster(file.path(path.to.buffers, "mask_lowarctic_area_no_ice.grd"))
r.ha <- raster(file.path(path.to.buffers, "mask_higharctic_area_no_ice.grd"))

mask.ids <- c('r.full.area', 'r.from.sa', 'r.sa', 'r.from.la', 'r.la', 'r.ha')

## 2. compute for each scenario the nb of species lost/gain and the species richness by pixel
tab_ <- param.tab.job
## define a function that calculates the alpha, SG, SL and turnover maps
# calculate_alpha_gain_loss_turnover <- function(tab_){
  cat("\n ***")
  library(raster)
  library(dplyr)
  cat("\n> libraries loaded")
  div.fact_ <- tab_ %>% dplyr::select(-sp, -src_ras_file) %>%  distinct %>% nrow ## the nb of gcm x rcp to come back in nb species scale
  nb.sp_ <- tab_ %>% dplyr::select(sp) %>% distinct %>% nrow
  src.maps_ <- lapply(src.maps.files, function(f_ ) raster(f_, RAT = FALSE))
  cat("\n> src.maps loaded")
  ## src.maps are codded like:
  #   -2 if the given pixel is predicted to be lost by the species. 
  #   -1 if the given pixel is predicted to be stable for the species.
  #   0 is the given pixel was not occupied, and will not be in the future.
  #   1 if the given pixel was not occupied, and is predicted to be into the future.
  
  occ.maps.cur_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 1,
                                                               -1.5, -0.5, 1,
                                                               -0.5, 0.5,  0,
                                                               0.5, 1.5, 0)))
  occ.maps.cur_ <- occ.maps.cur_
  cat("\n> pres occ.maps porduced")
  
  occ.maps.fut_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 0,
                                                                    -1.5, -0.5, 1,
                                                                    -0.5, 0.5,  0,
                                                                    0.5, 1.5, 1)))
  occ.maps.fut_ <- occ.maps.fut_ 
  cat("\n> fut occ.maps porduced")
  
  alphadiv.map.cur_ <- sum(raster::stack(occ.maps.cur_)) / div.fact_
  cat("\n> pres alphadiv.map porduced")
  
  alphadiv.map.fut_ <- sum(raster::stack(occ.maps.fut_)) / div.fact_
  cat("\n> fut alphadiv.map porduced")
  
  gain.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 0,
                                                               -1.5, -0.5, 0,
                                                               -0.5, 0.5,  0,
                                                               0.5, 1.5, 1)))
  gain.map_ <- sum(raster::stack(gain.maps_)) / div.fact_
  cat("\n> gain.map produced")
  
  lost.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 1,
                                                                -1.5, -0.5, 0,
                                                                -0.5, 0.5,  0,
                                                                0.5, 1.5, 0)))
  lost.map_ <- sum(raster::stack(lost.maps_)) / div.fact_
  cat("\n> lost.map produced")
  
  turnover.map_ <- (lost.map_ + gain.map_) / (gain.map_ + alphadiv.map.cur_)
  cat("\n> turnover.map produced")
  
  out.stack_ <- raster::brick(alphadiv.map.cur_,
                              alphadiv.map.fut_,
                              alphadiv.map.fut_ - alphadiv.map.cur_,
                              gain.map_, 
                              lost.map_, 
                              gain.map_ / alphadiv.map.cur_ * 100,
                              lost.map_ / alphadiv.map.cur_ * 100,
                              turnover.map_) * r.full.area
  names(out.stack_) <- c("alphadiv.cur", "alphadiv.fut", "alphadiv.change", "gain", "lost", "pct.gain", "pct.lost", "turnover")
  
  ## save the output stack on hard drive
  stack.file.name_ <- file.path(out.dir.path, paste0("alpha_gain_lost_turnover_", job.id, ".grd"))
  cat("\n> out.stack saved as", stack.file.name_)
  writeRaster(out.stack_, filename = stack.file.name_, overwrite = TRUE)
  
  ## compute the summary stats area by area
  
  ############### HERE ########################
  ## compute the boxplot stats by area
  out.list <- vector(length = length(mask.ids), mode = 'list')
  names(out.list) <- mask.ids
  for(m_ in names(out.list)){ ## test m_ <- "r.sa"
    stk.tmp_ <- mask(out.stack_, get(m_))
    bp_stat_ <- boxplot(stk.tmp_, na.rm = TRUE)
    out_tab_ <- data.frame(t(bp_stat_$stats))
    colnames(out_tab_) <- c("ymin", "lower", "middle", "upper", "ymax")
    out_tab_$area <- switch(m_,
                            r.full.area = "full_area", 
                            r.from.sa = "from_sub_arctic", 
                            r.sa = "sub_arctic", 
                            r.from.la = "from_low_arctic", 
                            r.la = "low_arctic", 
                            r.ha = "high_arctic")
    out_tab_$n <- bp_stat_$n
    out_tab_$metric <- bp_stat_$names
    ## remove the turnover that has no sens here
    # out_tab_ <- out_tab_ %>% filter(metric != "turnover")
    out.list[[m_]] <- out_tab_
  }
  
  out.tab_ <- bind_rows(out.list)
  
  out.tab_ <- out.tab_ %>% 
    mutate(filt = param.tab$filt, biointer = param.tab$biointer, gf = param.tab$gf,
           alpha_gain_lost_turnover_ras_file = stack.file.name_, div_fact = div.fact_, n_sp = nb.sp_)

  write.table(out.tab_, file = sub(".grd$", ".txt", stack.file.name_), sep = "\t", col.names = FALSE, row.names = TRUE)
  
  q("no")
  ########################################################
  
#   cat("\n ***")
#   return(stack.file.name_)
# }

# ### test
# tab_ <- gg.dat %>% 
#   filter(scenario.biomod == scenario.biomod[1],
#          biotic.inter == biotic.inter[1],
#          dispersal.filter == dispersal.filter[1],
#          gcm == gcm[1],
#          rcp == rcp[1])
# ### end of test 

# 
# if(n.cores <= 1){
#   ## sequential version
#   gg.calc <- gg.dat %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp) %>%
#     do(stack.file.name = calculate_alpha_gain_loss_turnover(.))
# } else{
#   ## parallel version
#   clust <- create_cluster(cores = n.cores, quiet = FALSE)
#   clusterExport(clust,c("calculate_alpha_gain_loss_turnover", "out.dir.path", "src.maps.files"))
#   gg.dat.part <- partition(gg.dat, scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, cluster = clust)
#   gg.calc <- gg.dat.part %>% do(stack.file.name = calculate_alpha_gain_loss_turnover(.))
#   stopCluster(clust)
# }


# ## do the same table grouped by growth form
# 
# if(n.cores <= 1){
#   ## sequential version
#   gg.calc.gf <- gg.dat %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, growth.form) %>%
#     do(stack.file.name = calculate_alpha_gain_loss_turnover(.))
# } else {
# 
# }
# 
# 
# save(gg.calc.gf, file = file.path(out.dir.path, "gg.calc.gf.RData"))


##' @note
##'  - has to be designed in parallel way with multidplyr package
##'  - check why raster stacks are not well saved (bands insted of layers)
##'  - add the path to tmp raster dir to prevent from filling /tmp of the cluster

##' ------------------------------------------------------------------------------------


###########################################################################
## create parameter file

## the param file is based on the output of speces_range_change_baseline.R
out.dir <- "/work/georges/BRISCA/grid_params/"
params <- read.table("/work/georges/BRISCA/SRC_baseline_tabs_2017-04-26.txt", 
                     sep = "\t", stringsAsFactors = FALSE, header = TRUE)

params <- params %>% dplyr::select(filt, biointer, gf) %>% distinct
write.table(params, file = file.path(out.dir, "params_alphadiv_2017-04-25.txt"), sep = "\t", col.names = T)



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
