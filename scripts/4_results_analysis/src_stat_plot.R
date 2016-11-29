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
.libPaths("J:/People/Damien/RLIBS") ## on brisca cluster
library(dplyr)
library(ggplot2)
library(tidyr)

## define the main paths to data
briscahub.dir <- "J:/People/Damien/BRISCA/briscahub/" ## on brisca cluster
src.tab.path <- "I:/C_Write/Damien/BRISCA/backup_idiv_cluster/SRC_baseline_tabs_new.RData"
out.dir.path <-"I:/C_Write/Damien/BRISCA/figures/2016-11-2_"

dir.create(out.dir.path, recursive = TRUE, showWarnings = FALSE)

##' ## get and reshape the data

## laod src scores table
load(src.tab.path)
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
unique(src.tab$growth.form)

# gg.dat <- src.tab %>% 
#   filter(area %in% c("r.from.sa", "r.sa", "r.la", "r.ha")) %>%
#   mutate(rcp = sub("_2080.*$", "", scenario.clim),
#          gcm = sub("_(no|max)_disp.*$", "", sub(".*_2080_", "", scenario.clim)),
#          biotic.inter = sub(paste0("^.*(", paste(unique(gcm), collapse="|"), ")"), "", scenario.clim),
#          dispersal.filter = sub("^.*TSSbin", "", tools::file_path_sans_ext(file.pattern)),
#          scenario.biomod = sub("_final", "", sub("Biomod_", "", scenario.biomod)))
# ## change dispersal filter labels
# gg.dat$dispersal.filter[gg.dat$dispersal.filter == ""] <- "unlimited"
# gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_ch"] <- "convex_hull"
# gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_no_disp_invdist"] <- "no"
# gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_min_disp_invdist"] <- "minimal"
# gg.dat$dispersal.filter[gg.dat$dispersal.filter == "_filt_max_disp_invdist"] <- "maximal"
# # gg.dat <- gg.dat %>% filter(is.element(dispersal.filter, c("minimal", "maximal", "unlimited")))
# gg.dat <- gg.dat %>% filter(is.element(dispersal.filter, c("no", "minimal", "maximal", "unlimited")))
# 
# ## change biointeraction labels
# gg.dat$biotic.inter[gg.dat$biotic.inter == ""] <- "no"
# gg.dat$biotic.inter[gg.dat$biotic.inter == "_no_disp_invdist"] <- "low"
# gg.dat$biotic.inter[gg.dat$biotic.inter == "_max_disp_invdist"] <- "high"
# ## change levels order
# gg.dat$biotic.inter <- factor(gg.dat$biotic.inter, levels =  c("no", "low", "high"))
# gg.dat$scenario.biomod <- factor(gg.dat$scenario.biomod, levels = c("pure_climate", "climate_and_biointer", "pure_climate_filtered", "climate_and_biointer_filtered"))
# gg.dat$dispersal.filter <- factor(gg.dat$dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))

gg.dat <- src.tab %>% mutate(dispersal.filter = replace(dispersal.filter, which(is.na(dispersal.filter)), "no"))
gg.dat$dispersal.filter <- factor(gg.dat$dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))
gg.dat$biotic.inter <- factor(gg.dat$biotic.inter, levels =  c("no", "low", "high"))

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
gg.dat %>% ungroup %>% filter(!is.na(nb.stable0.pix)) %>% group_by(biotic.inter, dispersal.filter) %>% summarise(n = n())

head(gg.dat)


## first version
# gg.plot <- ggplot(gg.dat, aes(scenario.biomod, species.range.change, fill = biotic.inter, color = dispersal.filter)) +
#   geom_boxplot(outlier.colour = NA) + facet_grid(gcm ~ rcp) + 
#   coord_cartesian(ylim = c(-100, ifelse(same.baseline, 4500, 500))) + ## with same baselines or not change graph scale
#   scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "biotic interactions")) + 
#   scale_color_brewer(palette = "Dark2", guide = guide_legend(title = "dispersal distance")) + 
#   xlab("scenario") + ylab("species range change (%)") + 
#   gg.theme
# ggsave(file.path(out.dir.path, ifelse(same.baseline, "src_baseline_stat_from_low_arctic.png", "src_stat_from_low_arctic.png")), gg.plot, width = 297, height = 210, units = 'mm')


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

## to do the sample comparaison
xx <- gg.dat %>% ungroup %>% group_by(metric.name, area) %>%
  # summarize(n = n()) 
  # do(data.frame(table(.[is.finite(.$metric.val), ]$dispersal.and.biotic)))
  # do(data.frame(species.to.remove = unique(.$species[is.na(.$metric.val)])))
  do(data.frame(.[!(.$species %in% unique(.$species[!is.finite(.$metric.val)])), ])) %>%
  arrange(metric.name, area, metric.name, rcp, gcm, species)


gg.dat.t.test_ <- xx %>% ungroup %>% select(rcp, gcm, species, scenario.biomod, dispersal.filter, biotic.inter, metric.name, area, metric.val) %>%
  group_by(metric.name, area) %>% mutate(dispersal.and.biotic = factor(paste0(dispersal.filter, "_", biotic.inter))) %>%
  do(data.frame(try(pairwise.wilcox.test(.$metric.val, .$dispersal.and.biotic, paired = TRUE)$p.value %>% data.frame %>% 
                  mutate(var1.name = rownames(.)) %>% gather(var2.name, p.value, - var1.name))))



gg.dat.t.test_ %>% ungroup %>% group_by(area, metric.name) %>% summarise(n = sum(is.finite(p.value)))
gg.dat.t.test_ %>% filter(p.value > 0.05) %>% View

xxxx <- xx %>% filter(area == "Full arctic", metric.name =="SR Gain (%)")  %>%
  mutate(dispersal.and.biotic = factor(paste0(dispersal.filter, "_", biotic.inter)))
table(na.omit(xxxx$dispersal.and.biotic))

pairwise.wilcox.test(xxxx$metric.val, xxxx$dispersal.and.biotic, paired = TRUE)$p.value %>% data.frame %>% 
  mutate(var1.name = rownames(.)) %>% gather(var2.name, p.value, - var1.name)
# gg.plot <- ggplot(gg.dat, aes(1, metric.val, fill = dispersal.filter, linetype = biotic.inter)) +
#   geom_boxplot(outlier.colour = NA) + facet_grid(metric.name ~ area, scale = 'free_y') + 
#   coord_cartesian(ylim = c(-100, 4500)) + ## with same baselines or not change graph scale
#   scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
#   scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
#   xlab("") + ylab("") +
#   gg.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# gg.plot

gg.plot <- ggplot(gg.dat.no.ol, aes(1, fill = dispersal.filter, linetype = biotic.inter)) +
  geom_boxplot(aes(lower = X2, middle = X3, upper = X4, ymin = X1, ymax = X5), 
               stat = "identity", outlier.colour = NA, position = position_dodge(1.5)) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

gg.plot
x11()


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
gg.plot

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
gg.plot


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
gg.plot

ggsave(file.path(out.dir.path, "figXa_pseudo_log.png"), gg.plot, width = 320, height = 210, units = 'mm')


### extra tests to check that 
load(src.tab.path)
## add the no disp pattern
src.tab <- src.tab %>% mutate(dispersal.filter = replace(dispersal.filter, grepl("_filt_no_disp_invdist.grd", file.pattern), "no"))

## check the trends due to dispersal filter and biotic interactions
src.tab %>% group_by(species, rcp, gcm, area, biotic.inter, dispersal.filter) %>%
  select(src) %>% mutate(n = n()) %>% data.frame %>% head

# get_dispersal_rank <- function(x){
#   xx_ <- c(x$no, x$minimal, x$maximal, x$unlimited)
#   df <- data.frame(r_no = NA, r_minimal = NA, r_maximal = NA, r_unlimited = NA)
#   if(all(!is.na(xx_))) df[1,] <- rank(xx_)
#   return(df)
# }
# 
# dip_rank_tab <- src.tab %>% mutate(dispersal.filter = factor(dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))) %>%
#   group_by(species, rcp, gcm, area, biotic.inter) %>%
#   select(src, dispersal.filter) %>% spread(dispersal.filter, src) %>% 
#   rowwise() %>% do(get_dispersal_rank(.))

disp_rank_tab <- src.tab %>% mutate(dispersal.filter = factor(dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))) %>%
  group_by(species, rcp, gcm, area, biotic.inter) %>%
  select(src, dispersal.filter) %>% do(data.frame(src = .$src, 
                                                  dispersal.filter= .$dispersal.filter, 
                                                  src_rank = rank(.$src, ties.method = "random", na.last = "keep")))
ggplot(data = disp_rank_tab) +
  geom_bar(aes(x = dispersal.filter, fill = factor(src_rank))) +
  scale_fill_brewer()

biotic_rank_tab <- src.tab %>% mutate(dispersal.filter = factor(dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))) %>%
  group_by(species, rcp, gcm, area, dispersal.filter) %>%
  select(src, biotic.inter) %>% do(data.frame(src = .$src, 
                                              biotic.inter= .$biotic.inter,
                                              src_rank = rank(.$src, ties.method = "random", na.last = "keep")))


ggplot(data = biotic_rank_tab) +
  geom_bar(aes(x = biotic.inter, fill = factor(src_rank))) +
  scale_fill_brewer()

