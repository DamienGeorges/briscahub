##' ---
##' @title Construct the alpha diversity, gain and loss summary tables to 
##'   construct the article boxplots (fig2)
##' @author damien georges
##' @date 2016-05-11
##' @licence GPL-2
##' ---

##' **note: this script has to be run after alphadiv_gain_and_loss_maps.R**

##' ## Initialisation

rm(list = ls())

## set some parameters
same.baseline <- TRUE ## do we consider the same baseline (climate filtered no dispersal) as a baseline or 
## each scenario current prediction as baseline
machine <- "signe_cluster" # "sdiv" ## the name of the machine the script will run on
n.cores <- 1 ## number of resuired cores

## define the main paths to data
if(machine == "leca97"){
} else if (machine == "pinea"){
} else if (machine == "sdiv"){
} else if (machine == "signe_cluster"){
  .libPaths( "J:/People/Damien/RLIBS")
  briscahub.dir <- "J://People/Damien/BRISCA/briscahub/"
  alpha.div.map.path <-  paste0("I://C_Write/Damien/BRISCA/backup_idiv_cluster/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack_by_growth_form")
  param.tab.path <- paste0("I://C_Write/Damien/BRISCA/backup_idiv_cluster/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack_by_growth_form/gg.calc.RData")
  out.dir.path <- paste0("I://C_Write/Damien/BRISCA/backup_idiv_cluster/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack_by_growth_form_stat")
} else stop("\n> unknow machine!")

setwd(file.path(briscahub.dir, "workdir"))
## load needed libraries
library(raster)
library(dplyr)
library(multidplyr)
library(parallel)
library(ggplot2)
library(tidyr)

dir.create(out.dir.path, showWarnings = FALSE, recursive =TRUE)


## load couple of masks to compute stats locally
r.full.area <- raster("I://C_Write/Damien/BRISCA/data/mask_raster_arctic_area_2016-08-22/mask_full_area_no_ice.grd")
r.from.sa <- raster("I://C_Write/Damien/BRISCA/data/mask_raster_arctic_area_2016-08-22/mask_from_subarctic_area_no_ice.grd")
r.sa <- raster("I://C_Write/Damien/BRISCA/data/mask_raster_arctic_area_2016-08-22/mask_subarctic_area_no_ice.grd")
r.from.la <- raster("I://C_Write/Damien/BRISCA/data/mask_raster_arctic_area_2016-08-22/mask_from_lowarctic_area_no_ice.grd")
r.la <- raster("I://C_Write/Damien/BRISCA/data/mask_raster_arctic_area_2016-08-22/mask_lowarctic_area_no_ice.grd")
r.ha <- raster("I://C_Write/Damien/BRISCA/data/mask_raster_arctic_area_2016-08-22/mask_higharctic_area_no_ice.grd")

mask.ids <- c('r.full.area', 'r.from.sa', 'r.sa', 'r.from.la', 'r.la', 'r.ha')

## load the formal alphadiv ref tab
load(param.tab.path)
# ## add the region field
# gg.calc <- gg.calc %>% mutate(stack.file.name = unlist(stack.file.name)) %>% 
#   data.frame %>% cbind(area = rep(mask.ids, each = nrow(gg.calc)))

## We will aggregate resutls accross GCMs and RCPs
mean.alpha.div.dir <- file.path(out.dir.path, "mean_rcp_gcm_alphadiv_ras")
dir.create(mean.alpha.div.dir, showWarnings = FALSE, recursive = TRUE)

## test
x <- gg.calc %>% filter(scenario.biomod == "pure_climate",
                        biotic.inter == "no", 
                        dispersal.filter == "unlimited",
                        growth.form == "Dwarf Shrubs"#,
                        # area == "r.full.area"
                        )
## end test
mean_alphadiv <- function(x){
  scenario.biomod_ <- unique(x$scenario.biomod)
  biotic.inter_ <- unique(x$biotic.inter)
  dispersal.filter_ <- unique(x$dispersal.filter)
  growth.form_ <- unique(x$growth.form)
  # area_ <- unique(x$area)
  stk.files_ <- x$stack.file.name
  n.stk_ <- length(stk.files_)
  stk.name_ <- file.path(mean.alpha.div.dir, paste0("mean_alphadiv_stk_across_rcp_gcm__", scenario.biomod_, "__", biotic.inter_, "__", dispersal.filter_, "__", growth.form_, ".grd"))
  stk.list_ <- lapply(stk.files_, stack, RAT = FALSE)
  # stk.list_$fun <- mean
  # stk.list_$filename <- stk.name_
  # stk.mean_ <- do.call(overlay, tail(stk.list_, 5)) 
  stk.sum_ <- stk.list_[[1]]
  for(l in 2:n.stk_) stk.sum_ <- stk.sum_ + stk.list_[[l]]
  stk.mean_ <- stk.sum_ / n.stk_
  writeRaster(stk.mean_, filename = stk.name_, overwrite = TRUE)
  return(data.frame(stk.name = stk.name_))
}

## do the mean of stack across gcm and rcps
gg.mean.alphadiv <- gg.calc %>% ungroup %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, growth.form) %>%
  do(data.frame(mean_alphadiv(.)))

save(gg.mean.alphadiv, file = file.path(mean.alpha.div.dir, "gg.mean.alphadiv.RData"))

## compute the pix stats by area
## test
x <- gg.mean.alphadiv %>% filter(scenario.biomod == "pure_climate",
                                 biotic.inter == "no",
                                 dispersal.filter == "unlimited")
## end test
bp_stat_alphadiv <- function(x){
  scenario.biomod_ <- unique(x$scenario.biomod)
  biotic.inter_ <- unique(x$biotic.inter)
  dispersal.filter_ <- unique(x$dispersal.filter)
  ## do the sum of alphadiv, gain and loss maps (note: sum of turnover has no sens here)
  stk.files_ <- x$stk.name
  n.stk_ <- length(stk.files_)
  stk.list_ <- lapply(stk.files_, stack, RAT = FALSE)
  ## add the alphadiv.change layer
  stk.list_ <- lapply(stk.list_, function(s_){ s_[['alphadiv.change']] <-  s_[['gain']] - s_[['lost']]; return(s_)})
  ## add the %loss and %gain layers
  stk.list_ <- lapply(stk.list_, function(s_){ 
    cur.alpha_ <- s_[['alphadiv']] - s_[['alphadiv.change']]
    s_[['pc.gain']] <-  (s_[['gain']] / cur.alpha_) * 100
    s_[['pc.lost']] <-  (s_[['lost']] / cur.alpha_) * 100
    return(s_)})
  stk.sum_ <- stk.list_[[1]]
  for(l in 2:n.stk_) stk.sum_ <- stk.sum_ + stk.list_[[l]]
  ## compute the boxplot stats by area
  out.list <- vector(length = length(mask.ids), mode = 'list')
  names(out.list) <- mask.ids
  for(m_ in names(out.list)){ ## test m_ <- "r.sa"
    stk.tmp_ <- mask(stk.sum_, get(m_))
    bp_stat_ <- boxplot(stk.tmp_, na.rm = TRUE)
    out_tab_ <- data.frame(t(bp_stat_$stats))
    colnames(out_tab_) <- c("ymin", "lower", "middle", "upper", "ymax")
    out_tab_$area <- m_
    out_tab_$n <- bp_stat_$n
    out_tab_$metric <- bp_stat_$names
    ## remove the turnover that has no sens here
    out_tab_ <- out_tab_ %>% filter(metric != "turnover")
    out.list[[m_]] <- out_tab_
  }
  out.tab <- bind_rows(out.list)
  return(out.tab)
}

# gg.bp.stat <- gg.mean.alphadiv %>% ungroup %>% group_by(scenario.biomod, biotic.inter, dispersal.filter) %>%
#   do(data.frame(bp_stat_alphadiv(.)))
# 
# gg.bp.stat %>% data.frame %>% head
# save(gg.bp.stat, file = file.path(mean.alpha.div.dir, "gg.bp.stat.RData"))

## do teh same by growth form
bp_stat_alphadiv_gf <- function(x){
  scenario.biomod_ <- unique(x$scenario.biomod)
  biotic.inter_ <- unique(x$biotic.inter)
  dispersal.filter_ <- unique(x$dispersal.filter)
  growth.form_ <- unique(x$growth.form)
  cat("\n ----------------------------------")
  cat("\n> scenario.biomod_ : ", scenario.biomod_)
  cat("\n> biotic.inter_ : ", biotic.inter_)
  cat("\n> dispersal.filter_ : ", dispersal.filter_)
  cat("\n> growth.form_ : ", growth.form_)
  
  ## do the sum of alphadiv, gain and loss maps (note: sum of turnover has no sens here)
  stk.files_ <- x$stk.name
  n.stk_ <- length(stk.files_)
  cat("\n> n.stk_ : ", n.stk_)
  stk.list_ <- lapply(stk.files_, stack, RAT = FALSE)
  ## add the alphadiv.change layer
  stk.list_ <- lapply(stk.list_, function(s_){ s_[['alphadiv.change']] <-  s_[['gain']] - s_[['lost']]; return(s_)})
  ## add the %loss and %gain layers
  stk.list_ <- lapply(stk.list_, function(s_){ 
    cur.alpha_ <- s_[['alphadiv']] - s_[['alphadiv.change']]
    s_[['pc.gain']] <-  (s_[['gain']] / cur.alpha_) * 100
    s_[['pc.lost']] <-  (s_[['lost']] / cur.alpha_) * 100
    return(s_)})
  stk.sum_ <- stk.list_[[1]]
  cat("\n> length(stk.list_) : ", length(stk.list_))
  if(length(stk.list_) > 1){
    for(l in 2:n.stk_) stk.sum_ <- stk.sum_ + stk.list_[[l]]
  }
  ## compute the boxplot stats by area
  out.list <- vector(length = length(mask.ids), mode = 'list')
  names(out.list) <- mask.ids
  for(m_ in names(out.list)){ ## test m_ <- "r.sa"
    stk.tmp_ <- mask(stk.sum_, get(m_))
    bp_stat_ <- boxplot(stk.tmp_, na.rm = TRUE)
    out_tab_ <- data.frame(t(bp_stat_$stats))
    colnames(out_tab_) <- c("ymin", "lower", "middle", "upper", "ymax")
    out_tab_$area <- m_
    out_tab_$n <- bp_stat_$n
    out_tab_$metric <- bp_stat_$names
    ## remove the turnover that has no sens here
    out_tab_ <- out_tab_ %>% filter(metric != "turnover")
    out.list[[m_]] <- out_tab_
  }
  out.tab <- bind_rows(out.list)
  return(out.tab)
}

try_bp_stat_alphadiv_gf <- function(x){
  out <- try(bp_stat_alphadiv_gf(x))
  if(inherits(out, 'try-error')) out <- NULL
  return(out)
}
gg.bp.gf.stat <- gg.mean.alphadiv %>% ungroup %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, growth.form) %>%
  do(data.frame(try_bp_stat_alphadiv_gf(.)))

gg.bp.gf.stat %>% data.frame %>% head
save(gg.bp.gf.stat, file = file.path(mean.alpha.div.dir, "gg.bp.gf.stat.RData"))
