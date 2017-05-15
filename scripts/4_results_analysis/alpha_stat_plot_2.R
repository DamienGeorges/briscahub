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
# .libPaths("/home/georges/R/x86_64-pc-linux-gnu-library/3.2/")
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)

## define the main paths to data
briscahub.dir <- "~/BRISCA/briscahub/" ## on pinea
alpha.out.tab.dir <- "/work/georges/BRISCA/alphadiv_2017-05-08"
out.dir.path <-"/work/georges/BRISCA/figures/2017-05-15"
dir.create(out.dir.path, recursive = TRUE, showWarnings = FALSE)

## laod alpha scores table
alpha.out.tab.files <- list.files(alpha.out.tab.dir, ".txt", full.names = TRUE)
alpha.tab <- lapply(alpha.out.tab.files, read.table, sep = "\t", header = T, stringsAsFactors = FALSE) %>% bind_rows 
  
##' ## get and reshape the data

##' ## produce some graphs

## define an article fig friendly theme
gg.theme <- theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                  panel.border = element_rect(fill = NA, colour = 'grey80'),
                  panel.background = element_rect(fill = NA),
                  strip.background = element_rect(fill = NA, colour = 'grey80'),
                  legend.background = element_blank()) 
unique(alpha.tab$area)
unique(alpha.tab$gf)
unique(alpha.tab$biointer)


gg.dat <- alpha.tab
gg.dat <- gg.dat %>% 
  rename(X1 = ymin, 
         X2 = lower, 
         X3 = middle, 
         X4 = upper, 
         X5 = ymax, 
         dispersal.filter = filt,
         biotic.inter = biointer,
         biotic.inter.intensity = biointer_intensity,
         growth.form = gf) 

## remove the index we are not interested in
## and keep only All Shrub scenario
gg.dat <- gg.dat %>%
  filter(growth.form == "All shrub") %>%
  filter(metric %in% c("alphadiv.change", "pct.lost", "pct.gain"))

gg.dat$dispersal.filter <- factor(gg.dat$dispersal.filter, levels = c("no_dipersal", "max_dipersal", "unlimited_dipersal"), labels =  c("no", "maximal", "unlimited"))
gg.dat$biotic.inter <- factor(gg.dat$biotic.inter, levels =  c("no", "no_tree", "incl_tree"), labels = c("no", "without trees", "with trees"))
gg.dat$biotic.inter.intensity <- factor(gg.dat$biotic.inter.intensity, levels = c("no", "no_dipersal", "max_dipersal", "unlimited_dipersal"), labels =  c("-" ,"no", "maximal", "unlimited"))
gg.dat$metric <- factor(gg.dat$metric, levels = c("alphadiv.change", "pct.lost", "pct.gain"), labels = c("Alpha-div change", "species lost (%)", "species gain (%)"))


## remove some combination of params we are not interested in
# gg.dat <- gg.dat %>% filter(!(scenario.biomod == "climate_and_biointer_filtered" &  dispersal.filter == ""),
#                             !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "low" & dispersal.filter == "maximal"),
#                             !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "high" & dispersal.filter == "no"))


## change the area labels and order
gg.dat <- gg.dat %>% filter(area %in% c("from_sub_arctic", "sub_arctic", "low_arctic", "high_arctic"))
gg.dat$area <- factor(gg.dat$area, levels =  c("from_sub_arctic", "sub_arctic", "low_arctic", "high_arctic"),
                      labels = c("Full arctic", "Sub-arctic", "Low arctic", "High arctic"))


## check the number of combination computed
gg.dat %>% ungroup %>% group_by(biotic.inter, biotic.inter.intensity, dispersal.filter) %>% summarise(n = n())
head(gg.dat)


## first version


gg.plot <- ggplot(gg.dat %>% filter(biotic.inter != "with trees"), 
                  aes(1, fill = dispersal.filter, linetype = biotic.inter.intensity)) +  
  geom_boxplot(aes(lower = X2, middle = X3, upper = X4, ymin = X1, ymax = X5), 
               stat = "identity", outlier.colour = NA, position = position_dodge(1.5)) + 
  facet_grid(metric ~ area, scale = 'free_y') + 
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions intensity")) +
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## filter out the with trees dispersal scenario and add biotic interacion intesity
ggsave(file.path(out.dir.path, "fig1b_nt_bii.png"), gg.plot, width = 297, height = 210, units = 'mm')
## filter out the no trees dispersal scenario
ggsave(file.path(out.dir.path, "fig1b_wt_bii.png"), gg.plot %+% (gg.dat %>% filter(biotic.inter != "without trees")), width = 297, height = 210, units = 'mm')
