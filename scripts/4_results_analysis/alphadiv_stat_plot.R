##' ---
##' @title Plot the alpha diversity summaries
##' @descrtiption This script has been designed to produce geographical outputs
##' @author damien georges
##' @date 2016-11-05
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
alphadiv.tab.path <- "I://C_Write/Damien/BRISCA/backup_idiv_cluster/SRC_baseline_alpha_and_turnover_stack_by_growth_form_stat_new/mean_rcp_gcm_alphadiv_ras/gg.bp.gf.stat.RData"
out.dir.path <-"I:/C_Write/Damien/BRISCA/figures/2016-11-29"

dir.create(out.dir.path, recursive = TRUE, showWarnings = FALSE)

##' ## get and reshape the data

## laod alphadiv scores table
alphadiv.tab <- get(load(alphadiv.tab.path))


## load species ref table
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]

##' ## produce some graphs

## define an article fig friendly theme
gg.theme <- theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                  panel.border = element_rect(fill = NA, colour = 'grey80'),
                  panel.background = element_rect(fill = NA),
                  strip.background = element_rect(fill = NA, colour = 'grey80'),
                  legend.background = element_blank()) 

gg.dat <- alphadiv.tab %>% mutate(metric.name = metric) %>% 
  filter(area %in% c("r.from.sa", "r.sa", "r.la", "r.ha"),
         metric.name %in% c("alphadiv.change", "gain", "lost"))

gg.dat$area <- factor(gg.dat$area, levels =  c("r.from.sa", "r.sa", "r.la", "r.ha"), labels = c("Full arctic", "Sub-arctic", "Low arctic", "High arctic"))
gg.dat$metric.name = factor(gg.dat$metric.name, levels = c("alphadiv.change", "lost", "gain"), labels = c("AlphaDiv Change", "Nb Species Lost", "Nb Species Gained"))
gg.dat$dispersal.filter <- factor(gg.dat$dispersal.filter, levels =  c("no", "minimal", "maximal", "unlimited"))
gg.dat$biotic.inter <- factor(gg.dat$biotic.inter, levels =  c("no", "low", "high"))

## !!!! NOTE !!!!
## because we have some species that appearson the high arctic the percentage of gain is
## unlimited
## !!!! END NOTE !!!!



## check the number of combination computed
gg.dat %>% ungroup %>% group_by(biotic.inter, dispersal.filter) %>% summarise(n = n())
## fig1b asked by Anne and Signe on Oct 3

## reshape the table to produce the new graph
# 
# gg.dat.t.test_ <- xx %>% ungroup %>% select(rcp, gcm, species, scenario.biomod, dispersal.filter, biotic.inter, metric.name, area, metric.val) %>%
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

# pairwise.wilcox.test(xxxx$metric.val, xxxx$dispersal.and.biotic, paired = TRUE)$p.value %>% data.frame %>% 
#   mutate(var1.name = rownames(.)) %>% gather(var2.name, p.value, - var1.name)
# gg.plot <- ggplot(gg.dat, aes(1, metric.val, fill = dispersal.filter, linetype = biotic.inter)) +
#   geom_boxplot(outlier.colour = NA) + facet_grid(metric.name ~ area, scale = 'free_y') + 
#   coord_cartesian(ylim = c(-100, 4500)) + ## with same baselines or not change graph scale
#   scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
#   scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
#   xlab("") + ylab("") +
#   gg.theme + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
# gg.plot

gg.plot <- ggplot(gg.dat, aes(growth.form, fill = dispersal.filter, linetype = biotic.inter)) +
  geom_boxplot(aes(lower = lower, middle = middle, upper = upper, ymin = ymin, ymax = ymax), 
               stat = "identity", outlier.colour = NA, position = position_dodge(.9)) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
gg.plot


ggsave(file.path(out.dir.path, "figXb2.png"), gg.plot, width = 297, height = 210, units = 'mm')

## try to replace the alpha.div change by the raw alphadiv
gg.dat2 <- alphadiv.tab %>% mutate(metric.name = metric) %>% 
  filter(area %in% c("r.from.sa", "r.sa", "r.la", "r.ha"),
         metric.name %in% c("alphadiv", "gain", "lost"))

gg.dat2$area <- factor(gg.dat2$area, levels =  c("r.from.sa", "r.sa", "r.la", "r.ha"), labels = c("Full arctic", "Sub-arctic", "Low arctic", "High arctic"))
gg.dat2$metric.name = factor(gg.dat2$metric.name, levels = c("alphadiv", "lost", "gain"), labels = c("AlphaDiv", "Nb Species Lost", "Nb Species Gained"))

gg.plot <- ggplot(gg.dat2, aes(growth.form, fill = dispersal.filter, linetype = biotic.inter)) +
  geom_boxplot(aes(lower = lower, middle = middle, upper = upper, ymin = ymin, ymax = ymax), 
               stat = "identity", outlier.colour = NA, position = position_dodge(.9)) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
gg.plot


ggsave(file.path(out.dir.path, "figXb3.png"), gg.plot, width = 297, height = 210, units = 'mm')

## try to replace the alpha.div change by the raw alphadiv
gg.dat2 <- alphadiv.tab %>% mutate(metric.name = metric) %>% 
  filter(area %in% c("r.from.sa", "r.sa", "r.la", "r.ha"),
         metric.name %in% c("alphadiv.change", "pc.gain", "pc.lost"))

gg.dat2$area <- factor(gg.dat2$area, levels =  c("r.from.sa", "r.sa", "r.la", "r.ha"), labels = c("Full arctic", "Sub-arctic", "Low arctic", "High arctic"))
gg.dat2$metric.name = factor(gg.dat2$metric.name, levels = c("alphadiv.change", "pc.lost", "pc.gain"), labels = c("AlphaDiv Change", "Species Lost (%)", "Species Gained (%)"))

gg.plot <- ggplot(gg.dat2, aes(growth.form, fill = dispersal.filter, linetype = biotic.inter)) +
  geom_boxplot(aes(lower = lower, middle = middle, upper = upper, ymin = ymin, ymax = ymax), 
               stat = "identity", outlier.colour = NA, position = position_dodge(.9)) + 
  facet_grid(metric.name ~ area, scale = 'free_y') + 
  scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
  scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
  xlab("") + ylab("") +
  gg.theme + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
gg.plot


ggsave(file.path(out.dir.path, "figXb.png"), gg.plot, width = 297, height = 210, units = 'mm')

# ### make a try with a semilog scale
# ## to deal with the boxplot outliers
# 
# gg.dat.log.no.ol <- gg.dat %>% group_by(dispersal.filter, biotic.inter, metric.name, area) %>%
#   do(data.frame(t(boxplot.stats(log((.$metric.val + 100) / 100))$stats)))
# 
# gg.plot <- ggplot(gg.dat.log.no.ol, aes(1, fill = dispersal.filter, linetype = biotic.inter)) +
#   geom_boxplot(aes(lower = X2, middle = X3, upper = X4, ymin = X1, ymax = X5), 
#                stat = "identity", outlier.colour = NA, position = position_dodge(.9)) + 
#   facet_grid(metric.name ~ area, scale = 'free_y') + 
#   scale_fill_brewer(palette = "Blues", guide = guide_legend(title = "Dispersal distance")) + 
#   scale_linetype_discrete(guide = guide_legend(title = "Biotic interactions")) +
#   xlab("") + ylab("") +
#   gg.theme + theme(axis.text.x = element_text(angle = 0, hjust = .5, vjust = .5))
# gg.plot
# 
# ggsave(file.path(out.dir.path, "fig1_pseudo_log.png"), gg.plot, width = 297, height = 210, units = 'mm')
# 

