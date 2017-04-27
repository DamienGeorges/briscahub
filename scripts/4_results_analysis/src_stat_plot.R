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
src.maps.path <- "/work/georges/BRISCA/SRC_baseline_maps_2017-04-26"
src.out.tab.file <- "/work/georges/BRISCA/SRC_baseline_tabs_2017-04-26.txt"
param.tab.path <- "/work/georges/BRISCA/grid_params/params_alphadiv_2017-04-25.txt" ## on pinea
out.dir.path <-"I:/C_Write/Damien/BRISCA/figures/2017-04-27"
path.to.buffers <- "/home/georges/BRISCA/briscahub/data/mask_raster_arctic_area_2017-04-26"

dir.create(out.dir.path, recursive = TRUE, showWarnings = FALSE)

##' ## get and reshape the data

## laod src scores table
src.tab <- read.table(src.out.tab.file, sep = "\t", stringsAsFactors = FALSE, header = TRUE)

##' ## produce some graphs

## define an article fig friendly theme
gg.theme <- theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                  panel.border = element_rect(fill = NA, colour = 'grey80'),
                  panel.background = element_rect(fill = NA),
                  strip.background = element_rect(fill = NA, colour = 'grey80'),
                  legend.background = element_blank()) 
unique(src.tab$area)
unique(src.tab$gf)
unique(src.tab$biointer)

gg.dat <- src.tab
gg.dat <- gg.dat %>% 
  rename(nb.lost.pix = Loss, 
         nb.stable0.pix = Stable0, 
         nb.stable1.pix = Stable1, 
         nb.gain.pix = Gain, 
         percent.loss = PercLoss, 
         percent.gain = PercGain, 
         src = SpeciesRangeChange, 
         current.range.size = CurrentRangeSize, 
         future.range.size.no.disp = FutureRangeSize.NoDisp, 
         future.range.size.full.disp = FutureRangeSize.FullDisp,
         dispersal.filter = filt,
         biotic.inter = biointer,
         species = sp,
         growth.form = gf) 

gg.dat$dispersal.filter <- factor(gg.dat$dispersal.filter, levels = c("no_dipersal", "max_dipersal", "unlimited_dipersal"), labels =  c("no", "maximal", "unlimited"))
gg.dat$biotic.inter <- factor(gg.dat$biotic.inter, levels =  c("no", "no_tree", "incl_tree"), labels = c("no", "without trees", "with trees"))

## remove some combination of params we are not interested in
# gg.dat <- gg.dat %>% filter(!(scenario.biomod == "climate_and_biointer_filtered" &  dispersal.filter == ""),
#                             !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "low" & dispersal.filter == "maximal"),
#                             !(scenario.biomod == "climate_and_biointer_filtered" & biotic.inter == "high" & dispersal.filter == "no"))


## change the area labels and order
gg.dat <- gg.dat %>% filter(area %in% c("from_sub_arctic", "sub_arctic", "low_arctic", "high_arctic"))
gg.dat$area <- factor(gg.dat$area, levels =  c("from_sub_arctic", "sub_arctic", "low_arctic", "high_arctic"),
                      labels = c("Full arctic", "Sub-arctic", "Low arctic", "High arctic"))


## check the number of combination computed
gg.dat %>% ungroup %>% group_by(biotic.inter, dispersal.filter) %>% summarise(n = n())
colnames(gg.dat)
# [1] "layer_id"                 "Loss"                    
# [3] "Stable0"                  "Stable1"                 
# [5] "Gain"                     "PercLoss"                
# [7] "PercGain"                 "SpeciesRangeChange"      
# [9] "CurrentRangeSize"         "FutureRangeSize.NoDisp"  
# [11] "FutureRangeSize.FullDisp" "area"                    
# [13] "file.id"                  "sp"                      
# [15] "model"                    "rcp"                     
# [17] "gcm"                      "filt"                    
# [19] "biointer"                 "src_ras_file"            
# [21] "gf"  
head(gg.dat)


## first version

## fig1 asked by Anne and Signe on Oct 3
## reshape the table to produce the new graph
gg.dat <- gg.dat %>% gather(metric.name, metric.val, nb.lost.pix, nb.stable0.pix, nb.stable1.pix, nb.gain.pix, percent.loss, percent.gain, src, current.range.size, future.range.size.no.disp, future.range.size.full.disp) %>%
  mutate(bp.id = as.numeric(dispersal.filter) * 10 + as.numeric(biotic.inter))
gg.dat <- gg.dat %>% filter(metric.name %in% c('src', 'percent.loss', 'percent.gain')) %>%
  mutate(metric.name = factor(metric.name, levels = c('src', 'percent.loss', 'percent.gain'), labels = c("Species range change", "SR Loss (%)", "SR Gain (%)")))

## !!! the species that arrive in the high arctic have a infinite gain %
## that is kind of problematic for the boxplots.. lets try to remove non finite values
# gg.dat <- gg.dat %>% filter(is.finite(metric.val))

## to deal with the boxplot outliers
gg.dat.no.ol <- gg.dat %>% 
  filter(is.finite(metric.val)) %>%
  group_by(dispersal.filter, biotic.inter, metric.name, area) %>%
  do(data.frame(t(boxplot.stats(.$metric.val)$stats)))

#### TO BE REACTIVATED  ####

# ## to do the sample comparaison
# xx <- gg.dat %>% ungroup %>% group_by(metric.name, area) %>%
#   # summarize(n = n()) 
#   # do(data.frame(table(.[is.finite(.$metric.val), ]$dispersal.and.biotic)))
#   # do(data.frame(species.to.remove = unique(.$species[is.na(.$metric.val)])))
#   do(data.frame(.[!(.$species %in% unique(.$species[!is.finite(.$metric.val)])), ])) %>%
#   arrange(metric.name, area, metric.name, rcp, gcm, species)
# 
# gg.dat.t.test_ <- xx %>% ungroup %>% select(rcp, gcm, species, dispersal.filter, biotic.inter, metric.name, area, metric.val) %>%
#   group_by(metric.name, area) %>% mutate(dispersal.and.biotic = factor(paste0(dispersal.filter, "_", biotic.inter))) %>%
#   do(data.frame(try(pairwise.wilcox.test(.$metric.val, .$dispersal.and.biotic, paired = TRUE)$p.value %>% data.frame %>% 
#                   mutate(var1.name = rownames(.)) %>% gather(var2.name, p.value, - var1.name))))
# 
# 
# 
# gg.dat.t.test_ %>% ungroup %>% group_by(area, metric.name) %>% summarise(n = sum(is.finite(p.value)))
# gg.dat.t.test_ %>% filter(p.value > 0.05) %>% View
# 
# xxxx <- xx %>% filter(area == "Full arctic", metric.name =="SR Gain (%)")  %>%
#   mutate(dispersal.and.biotic = factor(paste0(dispersal.filter, "_", biotic.inter)))
# table(na.omit(xxxx$dispersal.and.biotic))
# 
# pairwise.wilcox.test(xxxx$metric.val, xxxx$dispersal.and.biotic, paired = TRUE)$p.value %>% data.frame %>% 
#   mutate(var1.name = rownames(.)) %>% gather(var2.name, p.value, - var1.name)
# # gg.plot <- ggplot(gg.dat, aes(1, metric.val, fill = dispersal.filter, linetype = biotic.inter)) +
# #   geom_boxplot(outlier.colour = NA) + facet_grid(metric.name ~ area, scale = 'free_y') + 
# #   coord_cartesian(ylim = c(-100, 4500)) + ## with same baselines or not change graph scale
# #   scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
# #   scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
# #   xlab("") + ylab("") +
# #   gg.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# # gg.plot
gg.plot <- ggplot(gg.dat.no.ol %>% data.frame, aes(1, fill = dispersal.filter, linetype = biotic.inter)) +  
  geom_boxplot(aes(lower = X2, middle = X3, upper = X4, ymin = X1, ymax = X5), 
               stat = "identity", outlier.colour = NA, position = position_dodge(1.5)) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# gg.plot
# x11()


ggsave(file.path(out.dir.path, "fig1a.png"), gg.plot, width = 297, height = 210, units = 'mm')

### make a try with a semilog scale
## to deal with the boxplot outliers

gg.dat.log.no.ol <- gg.dat %>% 
  filter(is.finite(metric.val)) %>% 
  group_by(dispersal.filter, biotic.inter, metric.name, area) %>%
  do(data.frame(t(boxplot.stats(log((.$metric.val + 100) / 100))$stats)))

gg.plot <- ggplot(gg.dat.log.no.ol, aes(1, fill = dispersal.filter, linetype = biotic.inter)) +
  geom_boxplot(aes(lower = X2, middle = X3, upper = X4, ymin = X1, ymax = X5), 
               stat = "identity", outlier.colour = NA, position = position_dodge(1.5)) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# gg.plot

ggsave(file.path(out.dir.path, "fig1a_pseudo_log.png"), gg.plot, width = 297, height = 210, units = 'mm')

### produce the same 2 graph but grouping by growth form -----------------------

## to deal with the boxplot outliers
gg.dat.gf.no.ol <- gg.dat %>% 
  filter(is.finite(metric.val)) %>%
  group_by(dispersal.filter, biotic.inter, metric.name, area, growth.form) %>%
  do(data.frame(t(boxplot.stats(.$metric.val)$stats)))

gg.dat.gf.log.no.ol <- gg.dat %>% 
  filter(is.finite(metric.val)) %>% 
  group_by(dispersal.filter, biotic.inter, metric.name, area, growth.form) %>%
  do(data.frame(t(boxplot.stats(log((.$metric.val + 100) / 100))$stats)))

gg.plot <- ggplot(gg.dat.gf.no.ol, aes(growth.form, fill = dispersal.filter, linetype = biotic.inter)) +
  geom_boxplot(aes(lower = X2, middle = X3, upper = X4, ymin = X1, ymax = X5), 
               stat = "identity", outlier.colour = NA, position = position_dodge(.9), varwidth=0.5) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
# gg.plot


ggsave(file.path(out.dir.path, "figXa.png"), gg.plot, width = 320, height = 210, units = 'mm')

### make a try with a semilog scale
gg.plot <- ggplot(gg.dat.gf.log.no.ol, aes(growth.form, fill = dispersal.filter, linetype = biotic.inter)) +
  geom_boxplot(aes(lower = X2, middle = X3, upper = X4, ymin = X1, ymax = X5), 
               stat = "identity", outlier.colour = NA, position = position_dodge(0.9)) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
# gg.plot

ggsave(file.path(out.dir.path, "figXa_pseudo_log.png"), gg.plot, width = 320, height = 210, units = 'mm')


### extra tests to check that 
load(src.tab.path)
## add the no disp pattern
src.tab <- src.tab %>% mutate(dispersal.filter = replace(dispersal.filter, grepl("_filt_no_disp_invdist.grd", file.pattern), "no"))

# ## check the trends due to dispersal filter and biotic interactions
# src.tab %>% group_by(species, rcp, gcm, area, biotic.inter, dispersal.filter) %>%
#   select(src, as.numeric(file.id)) %>% mutate(n = n()) %>% data.frame %>% head
# 
# # get_dispersal_rank <- function(x){
# #   xx_ <- c(x$no, x$minimal, x$maximal, x$unlimited)
# #   df <- data.frame(r_no = NA, r_minimal = NA, r_maximal = NA, r_unlimited = NA)
# #   if(all(!is.na(xx_))) df[1,] <- rank(xx_)
# #   return(df)
# # }
# # 
# # dip_rank_tab <- src.tab %>% mutate(dispersal.filter = factor(dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))) %>%
# #   group_by(species, rcp, gcm, area, biotic.inter) %>%
# #   select(src, dispersal.filter) %>% spread(dispersal.filter, src) %>% 
# #   rowwise() %>% do(get_dispersal_rank(.))
# 
# disp_rank_tab <- src.tab %>% mutate(dispersal.filter = factor(dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))) %>%
#   group_by(species, rcp, gcm, area, biotic.inter) %>%
#   select(src, dispersal.filter) %>% do(data.frame(src = .$src, 
#                                                   dispersal.filter= .$dispersal.filter, 
#                                                   src_rank = rank(.$src, ties.method = "random", na.last = "keep")))
# ggplot(data = disp_rank_tab) +
#   geom_bar(aes(x = dispersal.filter, fill = factor(src_rank))) +
#   scale_fill_brewer()
# 
# biotic_rank_tab <- src.tab %>% mutate(dispersal.filter = factor(dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))) %>%
#   group_by(species, rcp, gcm, area, dispersal.filter) %>%
#   select(src, biotic.inter) %>% do(data.frame(src = .$src, 
#                                               biotic.inter= .$biotic.inter,
#                                               src_rank = rank(.$src, ties.method = "random", na.last = "keep")))
# 
# 
# ggplot(data = biotic_rank_tab) +
#   geom_bar(aes(x = biotic.inter, fill = factor(src_rank))) +
#   scale_fill_brewer()
# 
