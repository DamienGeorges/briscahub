## recompute src stat table masking out iced area

##' ## Initialisation

rm(list = ls())
setwd("~/Work/BRISCA/workdir")

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
machine <- "signe_cluster" # "sdiv" ## the name of the machine the script will run on
n.cores <- 1 ## number of resuired cores

## define the main paths to data
if(machine == "leca97"){
  briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on leca97
  src.maps.path <-  paste0("~/Work/BRISCA/workdir/_SRC/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) ## on leca97
  param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt"
  out.dir.path <- paste0("~/Work/BRISCA/outputs/2016-08-18/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_statistic_table") ## on leca97
} else if (machine == "pinea"){
} else if (machine == "sdiv"){
} else if (machine == "signe_cluster"){
  .libPaths( "J:/People/Damien/RLIBS")
  briscahub.dir <- "J://People/Damien/BRISCA/briscahub/"
  src.maps.path <-  paste0("I://C_Write/Damien/BRISCA/backup_idiv_cluster/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) 
  param.tab.path <- "I://C_Write/Damien/BRISCA/parameters/grid_params/params_src.txt"
  out.dir.path <- paste0("I://C_Write/Damien/BRISCA/backup_idiv_cluster/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack_by_growth_form") ## on leca97
} else stop("\n> unknow machine!")

dir.create(out.dir.path, showWarnings = FALSE, recursive =TRUE)


## load species ref table
sp.tab <- read.table(file.path(briscahub.dir, "data/shrub.list_22082016.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

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

# param.tab <- param.tab %>% group_by(file.id) %>%
#   mutate(fut.file = paste0(mod.dir, "/", species, "/", proj.dir, "/individual_projections/", species, file.pattern), 
#          model =  sub("_.*$", "", sub(paste0(species, "_"), "", basename(fut.file))), 
#          scenario.full = sub(paste0(".*", species, "/"), "", dirname(dirname((fut.file)))),
#          scenario.clim = sub(".*RCP_", "RCP_", scenario.full),
#          scenario.biomod = basename(sub(paste0("/", species, ".*"), "", fut.file))
#   ) %>% ungroup


param.tab <- param.tab %>% rowwise() %>% #group_by(file.id) %>%
  mutate(
    fut.file = paste0(mod.dir, "/", species, "/", proj.dir, "/individual_projections/", species, file.pattern), 
    model =  sub("_.*$", "", sub(paste0(species, "_"), "", tail(unlist(strsplit(fut.file, split = "/")), 1))),
    scenario.full = sub(paste0(".*", species, "/"), "", head(tail(unlist(strsplit(fut.file, split = "/")), 3),1)),
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
## add the growth form attribute
gg.dat <- gg.dat %>% left_join(sp.tab %>% select(Biomod.name, Growth.form.isla) %>% rename(species = Biomod.name, growth.form = Growth.form.isla))

head(gg.dat)

## 2. compute for each scenario the nb of species lost/gain and the species richness by pixel

## check that no data is missing
gg.dat %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp) %>%
  summarize(nb.species = n()) %>% select(nb.species)

gg.dat %>% group_by(scenario.biomod, biotic.inter, dispersal.filter, gcm, rcp, growth.form) %>%
  summarize(nb.species = n()) %>% select(nb.species)

## load couple of masks to compute stats locally
r.full.area <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_full_area_no_ice.grd")
r.from.sa <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_from_subarctic_area_no_ice.grd")
r.sa <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_subarctic_area_no_ice.grd")
r.from.la <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_from_lowarctic_area_no_ice.grd")
r.la <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_lowarctic_area_no_ice.grd")
r.ha <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_higharctic_area_no_ice.grd")

mask.ids <- c('r.full.area', 'r.from.sa', 'r.sa', 'r.from.la', 'r.la', 'r.ha')


## recompute src summary stats for each species
tab_ <- gg.dat[25, ]
calculate_src_by_area(tab_)
## define a function that calculates SRC stats
calculate_src_by_area <- function(tab_){
  cat("\n ***")
  library(raster)
  library(dplyr)
  cat("\n> libraries loaded")
  src.maps.files_ <- src.maps.files[is.element(as.numeric(sub("_.*$", "", sub("src_baseline_", "", basename(src.maps.files)))), tab_$file.id)] 
  
  cat("\n> src.maps.file gotten (", length(src.maps.files_), ")")
  src.maps_ <- raster(src.maps.files_, RAT = FALSE)
  cat("\n> src.maps loaded")
  ## src.maps are codded like:
  #   -2 if the given pixel is predicted to be lost by the species. 
  #   -1 if the given pixel is predicted to be stable for the species.
  #   0 is the given pixel was not occupied, and will not be in the future.
  #   1 if the given pixel was not occupied, and is predicted to be into the future.
  
  out.tab_ <- NULL
  for(mask.id_ in mask.ids){
    mask_ <- get(mask.id_)
    src.map.masked_ <- mask(src.maps_, mask_)
    out.tab__ <- data.frame(area = mask.id_,
                           nb.non.na.pixs = sum(!is.na(src.map.masked_[])),
                           nb.lost.pix = sum(src.map.masked_[] == -2, na.rm = TRUE),
                           nb.stable0.pix = sum(src.map.masked_[] == 0 , na.rm = TRUE),
                           nb.stable1.pix = sum(src.map.masked_[] == -1 , na.rm = TRUE),
                           nb.gain.pix = sum(src.map.masked_[] == 1 , na.rm = TRUE)) %>%
      mutate(percent.loss = (100 * nb.lost.pix)/(nb.lost.pix + nb.stable1.pix),
             percent.gain = (100 * nb.gain.pix)/(nb.lost.pix + nb.stable1.pix),
             src = percent.gain - percent.loss,
             current.range.size = nb.lost.pix + nb.stable1.pix,
             future.range.size.no.disp = nb.stable1.pix,
             future.range.size.full.disp = nb.stable1.pix + nb.gain.pix)
    out.tab_ <- bind_rows(out.tab_, out.tab__)
  }
  
  out.tab_ <- tab_ %>% merge(out.tab_, by = NULL)
  return(out.tab_)
}

if(n.cores <= 1){
  ## sequential version
  gg.calc <- gg.dat %>% rowwise() %>%
    do(data.frame(output.table = calculate_src_by_area(.)))
} else{
  ## parallel version
  clust <- create_cluster(cores = n.cores, quiet = FALSE)
  clusterExport(clust,c("calculate_src_by_area", "out.dir.path", "src.maps.files", "mask.ids", 'r.full.area', 'r.from.sa', 'r.sa', 'r.from.la', 'r.la', 'r.ha'))
  gg.dat.part <- partition(gg.dat[1:25,], cluster = clust)
  gg.calc <- gg.dat.part %>% do(data.frame(output.table = calculate_src_by_area(.)))
  stopCluster(clust)
}

save(gg.calc, file = file.path(out.dir.path, "gg.calc.RData"))

write.table(gg.calc, file = file.path(out.dir.path, "src_stat_table_by_area.txt"), row.names = FALSE, col.names = TRUE, sep = "\t")
