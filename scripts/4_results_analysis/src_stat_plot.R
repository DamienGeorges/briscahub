##' ---
##' @title Plot the src summaries
##' @descrtiption This script has been designed to produce geographical outputs
##' @author damien georges
##' @date 2016-06-13
##' @licence GPL-2
##' ---

##' ## Initialisation

rm(list = ls())

## load needed libraries
library(dplyr)
library(ggplot2)
library(tidyr)

## set some parameters
same.baseline <- TRUE ## do we consider the same baseline (climate filtered no dispersal) as a baseline or 
                      ## each scenario current prediction as baseline


# ## define the main paths to data
# briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on leca97
# src.tab.path <- "~/Work/BRISCA/workdir/_SRC/SRC_tab.txt" ## on leca97
# param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt" ## on leca97
# out.dir.path <-"~/Work/BRISCA/figures/2016-06-13" ## on leca97

## define the main paths to data
briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on pinea
src.tab.path <- paste0("~/Work/BRISCA/workdir/_SRC/", ifelse(same.baseline, "SRC_baseline_tab.txt", "SRC_tab.txt")) ## on pinea
param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt" ## on pinea
out.dir.path <-"~/Work/BRISCA/figures/2016-06-13" ## on pinea


dir.create(out.dir.path, recursive = TRUE, showWarnings = FALSE)

##' ## get and reshape the data

## laod src scores table
src.tab <- read.table(src.tab.path, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
colnames(src.tab) <- c("disa", "stable0", "stable1", "gain", "perc.loss", "perc.gain", "species.range.change", 
                       "current.range.change", "future.range.size.no.disp", "future.range.size.full.disp",
                       "area", "species", "model", "scenario.clim", "scenario.biomod", "file.id", "sp.id")
head(src.tab)

## load species ref table
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]

## load grid campain parameters table
param.tab <- read.table(param.tab.path, sep = "\t", header = FALSE, stringsAsFactors = FALSE)
colnames(param.tab) <- c("mod.dir", "proj.dir", "file.pattern", "rcp", "gcm", "species")
## add the file id column
param.tab$file.id <- 1:nrow(param.tab)

## merge table in order to get the filtering info
src.tab <- src.tab %>% left_join(dplyr::select(param.tab, c(file.pattern, species, file.id)))


##' ## produce some graphs

## define an article fig friendly theme
gg.theme <- theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                  panel.border = element_rect(fill = NA, colour = 'grey80'),
                  panel.background = element_rect(fill = NA),
                  strip.background = element_rect(fill = NA, colour = 'grey80'),
                  legend.background = element_blank()) 

gg.dat <- src.tab %>% 
  # filter(area %in% c("from_sub_arctic", "from_low_arctic")) %>% 
  filter(area %in% c("from_low_arctic")) %>% 
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

gg.plot <- ggplot(gg.dat, aes(scenario.biomod, species.range.change, fill = biotic.inter, color = dispersal.filter)) +
  geom_boxplot(outlier.colour = NA) + facet_grid(gcm ~ rcp) + 
  coord_cartesian(ylim = c(-100, ifelse(same.baseline, 4500, 500))) + ## with same baselines or not change graph scale
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "biotic interactions")) + 
  scale_color_brewer(palette = "Dark2", guide = guide_legend(title = "dispersal distance")) + 
  xlab("scenario") + ylab("species range change (%)") + 
  gg.theme


ggsave(file.path(out.dir.path, ifelse(same.baseline, "src_baseline_stat_from_low_arctic.ps", "src_stat_from_low_arctic.ps")), gg.plot, width = 297, height = 210, units = 'mm')




