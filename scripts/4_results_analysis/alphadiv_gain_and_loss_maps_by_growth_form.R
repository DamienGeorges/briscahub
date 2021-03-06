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


## set some parameters
same.baseline <- TRUE ## do we consider the same baseline (climate filtered no dispersal) as a baseline or 
                      ## each scenario current prediction as baseline
machine <- "signe_cluster" # "sdiv" ## the name of the machine the script will run on
n.cores <- 1 ## number of resuired cores

## define the main paths to data
if(machine == "leca97"){
  briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on leca97
  src.maps.path <-  paste0("~/Work/BRISCA/workdir/_SRC/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) ## on leca97
  param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt"
  out.dir.path <- paste0("~/Work/BRISCA/outputs/2016-08-18/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack_by_growth_form") ## on leca97
} else if (machine == "pinea"){
} else if (machine == "sdiv"){
} else if (machine == "signe_cluster"){
  .libPaths( "J:/People/Damien/RLIBS")
  briscahub.dir <- "J://People/Damien/BRISCA/briscahub/"
  src.maps.path <-  "I://C_Write/Damien/BRISCA/backup_idiv_cluster/SRC_baseline_maps_new" 
  param.tab.path <- file.path(briscahub.dir, "data/params_src_new.RData")
  out.dir.path <- "I://C_Write/Damien/BRISCA/backup_idiv_cluster/SRC_baseline_alpha_and_turnover_stack_by_growth_form_new"
  # src.maps.path <-  paste0("I://C_Write/Damien/BRISCA/backup_idiv_cluster/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) 
  # param.tab.path <- "I://C_Write/Damien/BRISCA/parameters/grid_params/params_src.txt"
  # out.dir.path <- paste0("I://C_Write/Damien/BRISCA/backup_idiv_cluster/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack_by_growth_form") ## on leca97
  
  } else stop("\n> unknow machine!")

## load needed libraries
library(raster)
library(dplyr)
library(multidplyr)
library(parallel)
library(ggplot2)
library(tidyr)

dir.create(out.dir.path, showWarnings = FALSE, recursive =TRUE)


## load species ref table
sp.tab <- read.table(file.path(briscahub.dir, "data/shrub.list_22082016.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

## load grid campain parameters table
param.tab <- get(load(param.tab.path))
  
## 1. get the filenames of the SRC maps for each scenario that interest us
src.maps.files <- list.files(src.maps.path, ".grd$", full.names = TRUE)

## check if some maps are missing
computed.jobs <- as.numeric(sub("_.*$", "", sub("src_", "", basename(src.maps.files))))
missing.jobs <- setdiff(param.tab$file.id, computed.jobs)
param.tab[missing.jobs, ]

param.tab %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, growth.form) %>%
  summarize(nb.species = n()) %>% select(nb.species) %>% data.frame %>% head(50)

## define a function that calculates the alpha, SG, SL and turnover maps
calculate_alpha_gain_loss_turnover <- function(tab_){
  cat("\n ***")
  .libPaths( "J:/People/Damien/RLIBS")
  library(raster)
  library(dplyr, lib.loc = "J:/People/Damien/RLIBS")
  cat("\n> libraries loaded")
  src.maps.files_ <- src.maps.files[is.element(as.numeric(sub("_.*$", "", sub("src_", "", basename(src.maps.files)))), tab_$file.id)] 
  
  cat("\n> src.maps.file gotten (", length(src.maps.files_), ")")
  src.maps_ <- lapply(src.maps.files_, function(f_ ) raster(f_, RAT = FALSE))
  cat("\n> src.maps loaded")
  ## src.maps are codded like:
  #   -2 if the given pixel is predicted to be lost by the species. 
  #   -1 if the given pixel is predicted to be stable for the species.
  #   0 is the given pixel was not occupied, and will not be in the future.
  #   1 if the given pixel was not occupied, and is predicted to be into the future.
  
  fut.occ.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 0,
                                                               -1.5, -0.5, 1,
                                                               -0.5, 0.5,  0,
                                                               0.5, 1.5, 1)))
  
  cur.occ.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 1,
                                                                   -1.5, -0.5, 1,
                                                                   -0.5, 0.5,  0,
                                                                   0.5, 1.5, 0)))
  cat("\n> occ.maps porduced")
  fut.alphadiv.map_ <- sum(raster::stack(fut.occ.maps_))
  cur.alphadiv.map_ <- sum(raster::stack(cur.occ.maps_))
  cat("\n> alphadiv.map porduced")
  
  gain.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 0,
                                                               -1.5, -0.5, 0,
                                                               -0.5, 0.5,  0,
                                                               0.5, 1.5, 1)))
  gain.map_ <- sum(raster::stack(gain.maps_))
  cat("\n> gain.map produced")
  
  lost.maps_ <- lapply(src.maps_, function(r_) reclassify(r_, c(-2.5, -1.5, 1,
                                                                -1.5, -0.5, 0,
                                                                -0.5, 0.5,  0,
                                                                0.5, 1.5, 0)))
  lost.map_ <- sum(raster::stack(lost.maps_))
  cat("\n> lost.map produced")
  
  turnover.map_ <- (lost.map_ + gain.map_) / (gain.map_ + cur.alphadiv.map_)
  cat("\n> turnover.map produced")
  
  out.stack_ <- raster::brick(fut.alphadiv.map_, 
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
                                                    paste0(unique(tab_$rcp, collapse = "-")), "__",
                                                    paste0(unique(tab_$growth.form, collapse = "-")), ".grd"))
  cat("\n> out.stack saved as", stack.file.name_)
  writeRaster(out.stack_, filename = stack.file.name_, overwrite = TRUE)
  
  cat("\n ***")
  return(stack.file.name_)
}

# ### test
# tab_ <- param.tab %>%
#   filter(scenario.biomod == scenario.biomod[1],
#          biotic.inter == biotic.inter[1],
#          dispersal.filter == dispersal.filter[1],
#          gcm == gcm[1],
#          rcp == rcp[1])
# ### end of test

n.cores = 24
if(n.cores <= 1){
  ## sequential version
  gg.calc <- param.tab %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, growth.form) %>%
    do(stack.file.name = calculate_alpha_gain_loss_turnover(.))
} else{
  ## parallel version
  clust <- create_cluster(cores = n.cores, quiet = FALSE)
  clusterExport(clust,c("calculate_alpha_gain_loss_turnover", "out.dir.path", "src.maps.files"))
  clusterEvalQ(clust, {
    .libPaths( "J:/People/Damien/RLIBS")
    library(raster)
    library(dplyr, lib.loc = "J:/People/Damien/RLIBS")
    NULL
  })
  gg.dat.part <- partition(param.tab, scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, growth.form, cluster = clust)
  gg.calc <- gg.dat.part %>% do(stack.file.name = calculate_alpha_gain_loss_turnover(.))
  stopCluster(clust)
}

if(n.cores > 1){
  gg.calc.out <- param.tab %>% ungroup %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, growth.form) %>%
    summarize(stack.file.name = 
             file.path(out.dir.path,paste0("summaryStack__", 
                                    paste0(unique(scenario.biomod, collapse = "-")), "__",
                                    paste0(unique(biotic.inter, collapse = "-")), "__",
                                    paste0(unique(dispersal.filter, collapse = "-")), "__",
                                    paste0(unique(gcm, collapse = "-")), "__",
                                    paste0(unique(rcp, collapse = "-")), "__",
                                    paste0(unique(growth.form, collapse = "-")), ".grd"))
    )
  gg.calc <- gg.calc.out
}

save(gg.calc, file = file.path(out.dir.path, "gg.calc.RData"))

##' @note
##'  - has to be designed in parallel way with multidplyr package
##'  - check why raster stacks are not well saved (bands insted of layers)
##'  - add the path to tmp raster dir to prevent from filling /tmp of the cluster

##' ------------------------------------------------------------------------------------





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
