library(dplyr)

.libPaths( "J:/People/Damien/RLIBS")
briscahub.dir <- "J://People/Damien/BRISCA/briscahub/"
param.tab.path <- "I://C_Write/Damien/BRISCA/parameters/grid_params/params_src.txt"

## load species ref table
sp.tab <- read.table(file.path(briscahub.dir, "data/shrub.list_22082016.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

## load grid campain parameters table
param.tab <- read.table(param.tab.path, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
colnames(param.tab) <- c("mod.dir", "proj.dir", "file.pattern", "rcp", "gcm", "species")
## add the file id column
param.tab$file.id <- 1:nrow(param.tab)

param.tab.new <- param.tab
param.tab.new$mod.dir <- sub("/work/georges/BRISCA/", "/data/idiv_sdiv/brisca/results/", param.tab.new$mod.dir)

## remove the convex hull filters
param.tab.new <- param.tab.new %>% filter(!grepl("_filt_ch.grd", file.pattern))


param.tab.new <- param.tab.new %>% rowwise() %>% #group_by(file.id) %>%
  mutate(
    fut.file = paste0(mod.dir, "/", species, "/", proj.dir, "/individual_projections/", species, file.pattern), 
    model =  sub("_.*$", "", sub(paste0(species, "_"), "", tail(unlist(strsplit(fut.file, split = "/")), 1))),
    scenario.full = sub(paste0(".*", species, "/"), "", head(tail(unlist(strsplit(fut.file, split = "/")), 3),1)),
    scenario.clim = sub(".*RCP_", "RCP_", scenario.full),
    scenario.biomod = basename(sub(paste0("/", species, ".*"), "", fut.file))
  ) %>% ungroup

# param.tab %>% data.frame %>% head 
## keep only the jobs that are interesting for us
param.tab.new <- param.tab.new %>%  
  mutate(rcp = sub("_2080.*$", "", scenario.clim),
         gcm = sub("_(no|max)_disp.*$", "", sub(".*_2080_", "", scenario.clim)),
         biotic.inter = sub(paste0("^.*(", paste(unique(gcm), collapse="|"), ")"), "", scenario.clim),
         dispersal.filter = sub("^.*TSSbin", "", tools::file_path_sans_ext(file.pattern)),
         scenario.biomod = sub("_final", "", sub("Biomod_", "", scenario.biomod)))
## change dispersal filter labels
param.tab.new$dispersal.filter[param.tab.new$dispersal.filter == ""] <- "unlimited"
param.tab.new$dispersal.filter[param.tab.new$dispersal.filter == "_filt_ch"] <- "convex_hull"
param.tab.new$dispersal.filter[param.tab.new$dispersal.filter == "_filt_no_disp_invdist"] <- "no"
param.tab.new$dispersal.filter[param.tab.new$dispersal.filter == "_filt_min_disp_invdist"] <- "minimal"
param.tab.new$dispersal.filter[param.tab.new$dispersal.filter == "_filt_max_disp_invdist"] <- "maximal"
param.tab.new <- param.tab.new %>% filter(is.element(dispersal.filter, c("no", "minimal", "maximal", "unlimited")))
## change biointeraction labels
param.tab.new$biotic.inter[param.tab.new$biotic.inter == ""] <- "no"
param.tab.new$biotic.inter[param.tab.new$biotic.inter == "_no_disp_invdist"] <- "low"
param.tab.new$biotic.inter[param.tab.new$biotic.inter == "_max_disp_invdist"] <- "high"
## change levels order
param.tab.new$biotic.inter <- factor(param.tab.new$biotic.inter, levels =  c("no", "low", "high"))
param.tab.new$scenario.biomod <- factor(param.tab.new$scenario.biomod, levels = c("pure_climate", "climate_and_biointer", "pure_climate_filtered", "climate_and_biointer_filtered"))
param.tab.new$dispersal.filter <- factor(param.tab.new$dispersal.filter, levels =  c("minimal", "maximal", "unlimited"))
# ## remove some combination of params we are not interested in
# param.tab.new <- param.tab.new %>% filter(!(scenario.biomod == "climate_and_biointer_filtered" &  dispersal.filter == "no"),
#                             !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "low" & dispersal.filter == "maximal"),
#                             !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "high" & dispersal.filter == "minimal"))
## add the growth form attribute
param.tab.new <- param.tab.new %>% left_join(sp.tab %>% dplyr::select(Biomod.name, Growth.form.isla) %>% rename(species = Biomod.name, growth.form = Growth.form.isla))


param.tab.new %>% group_by(mod.dir) %>% summarize(n = n()) %>% data.frame
param.tab.new %>% group_by(mod.dir, proj.dir, file.pattern) %>% summarize(n = n()) %>% data.frame

write.table(param.tab.new, file = "data/params_src_new.txt", sep = "\t", row.names = F, col.names = F)
save(param.tab.new, file = "data/params_src_new.RData")
