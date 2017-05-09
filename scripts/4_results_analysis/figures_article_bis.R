## do fig 3 article and meta table


setwd("~/Work/BRISCA/workdir")
rm(list = ls())

output.dir <-"/work/georges/BRISCA/figures/2017-04-27/alphadiv_change_and_turnover_stats"
dir.create(output.dir, showWarnings = FALSE, recursive = TRUE)

## define an article fig friendly theme
gg.theme.1 <- theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                  panel.border = element_rect(fill = NA, colour = 'grey80'),
                  panel.background = element_rect(fill = NA),
                  strip.background = element_rect(fill = NA, colour = 'grey80'),
                  legend.background = element_blank()) 

gg.theme.2 <- theme(axis.text.x = element_text(),
                    panel.grid.major = element_line(colour = "grey90"),
                    panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                    panel.border = element_rect(fill = NA, colour = 'grey80'),
                    panel.background = element_rect(fill = NA),
                    strip.background = element_rect(fill = NA, colour = 'grey80'),
                    legend.background = element_blank(),
                    legend.title=element_blank()) 

## load the full table
src.tab <- read.table("~/Work/BRISCA/outputs/2016-08-18/SRC_baseline_statistic_table/src_stat_table_by_area.txt",
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
colnames(src.tab) <- sub("^output.table.", "", colnames(src.tab))

unique(src.tab$dispersal.filter)

## reshape the param table to fit with the other representations
## keep only the jobs that are interesting for us
gg.dat <- src.tab %>% filter(scenario.biomod != "climate_and_biointer") %>%
  filter(is.element(area, c("r.full.area", "r.from.sa", "r.from.la")))
gg.dat$area <- factor(gg.dat$area, levels =  c("r.full.area", "r.from.sa", "r.from.la"), labels = c("full area", "from subarctic", "from lowarctic"))

## change levels order
gg.dat$biotic.inter <- factor(gg.dat$biotic.inter, levels =  c("no", "low", "high"))
gg.dat$scenario.biomod <- factor(gg.dat$scenario.biomod, levels = c("pure_climate", "climate_and_biointer", "pure_climate_filtered", "climate_and_biointer_filtered"))
gg.dat$dispersal.filter <- factor(gg.dat$dispersal.filter, levels =  c("minimal", "maximal", "unlimited"))



## plot the mean src by class of shrubs / by scenario

gg.dat_ <- gg.dat %>% filter(area == "full area") %>% group_by(growth.form, scenario.biomod) %>%
  summarize(pc.src.pos  = sum(src > 0, na.rm = TRUE) / sum(!is.na(src)) * 100)

gg.plot <- ggplot(data = gg.dat_, aes(x = growth.form, y = pc.src.pos, fill = scenario.biomod)) +
  geom_bar(stat = 'identity', position = 'dodge') + gg.theme.2 + xlab("") + 
  ylab("% of species having a positive SRC") 
gg.plot
ggsave(filename = file.path(output.dir, "precentage.of.positive.src.by.growth.form.png"), plot = gg.plot)



## same but by area

gg.dat_ <- gg.dat %>% group_by(area, scenario.biomod) %>%
  summarize(pc.src.pos  = sum(src > 0, na.rm = TRUE) / sum(!is.na(src)) * 100)

gg.plot <- ggplot(data = gg.dat_, aes(x = area, y = pc.src.pos, fill = scenario.biomod)) +
  geom_bar(stat = 'identity', position = 'dodge') + gg.theme.2 + xlab("") + 
  ylab("% of species having a positive SRC") 
gg.plot
ggsave(filename = file.path(output.dir, "precentage.of.positive.src.by.area.png"), plot = gg.plot)


## do the same graphs but with boxplots
gg.dat_ <- gg.dat %>% filter(area == "full area") %>% group_by(growth.form, scenario.biomod)

sts_ <- gg.dat_ %>% do(data.frame(ymin = boxplot.stats(.$src)$stats[1] * 1.05, ymax = boxplot.stats(.$src)$stats[5] * 1.05))

gg.plot <- ggplot(data = gg.dat_, aes(x = growth.form, y = src, fill = scenario.biomod)) +
  geom_boxplot(outlier.colour = NA) + gg.theme.2 + xlab("") + coord_cartesian(ylim = c(min(sts_$ymin), max(sts_$ymax))) +
  ylab("Species Range Change (%)") 
gg.plot
ggsave(filename = file.path(output.dir, "boxplot.src.by.growth.form.png"), plot = gg.plot)


gg.dat_ <- gg.dat %>% group_by(area, scenario.biomod)

sts_ <- gg.dat_ %>% do(data.frame(ymin = boxplot.stats(.$src)$stats[1] * 1.05, ymax = boxplot.stats(.$src)$stats[5] * 1.05))

gg.plot <- ggplot(data = gg.dat_, aes(x = area, y = src, fill = scenario.biomod)) +
  geom_boxplot(outlier.colour = NA) + gg.theme.2 + xlab("") + coord_cartesian(ylim = c(min(sts_$ymin), max(sts_$ymax))) +
  ylab("Species Range Change (%)") 
gg.plot
ggsave(filename = file.path(output.dir, "boxplot.src.by.area.png"), plot = gg.plot)
















############################################
############################################
setwd("~/Work/BRISCA/workdir")
rm(list = ls())

## load needed libraries
library(raster)
library(dplyr)
library(multidplyr)
library(parallel)
library(ggplot2)
library(tidyr)

## define an article fig friendly theme
gg.theme.1 <- theme(axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
                    panel.grid.major = element_line(colour = "grey90"),
                    panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                    panel.border = element_rect(fill = NA, colour = 'grey80'),
                    panel.background = element_rect(fill = NA),
                    strip.background = element_rect(fill = NA, colour = 'grey80'),
                    legend.background = element_blank()) 

gg.theme.2 <- theme(axis.text.x = element_text(),
                    panel.grid.major = element_line(colour = "grey90"),
                    panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                    panel.border = element_rect(fill = NA, colour = 'grey80'),
                    panel.background = element_rect(fill = NA),
                    strip.background = element_rect(fill = NA, colour = 'grey80'),
                    legend.background = element_blank(),
                    legend.title=element_blank()) 

### do the same kind of graphs for the mean species richness diff and the mean turnover shift

## load the rasters of species richness and turnover
summ.stk.by.gf.path <- "~/Work/BRISCA/outputs/2016-08-18/SRC_baseline_alpha_and_turnover_stack_by_growth_form/"
summ.stk.all.path <- "~/Work/BRISCA/outputs/2016-08-18/SRC_baseline_alpha_and_turnover_stack/"


summary.stk.by.gf.fn <- list.files(summ.stk.by.gf.path, ".grd$")
summary.stk.all.fn <- list.files(summ.stk.all.path, ".grd$")

## climate filenames
stk.c.all.fn <- grep("pure_climate__no__unlimited", summary.stk.all.fn, value = TRUE)
stk.c.dwarf.fn <- grep("pure_climate__no__unlimited.*Dwarf Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.c.low.fn <- grep("pure_climate__no__unlimited.*Low Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.c.tall.fn <- grep("pure_climate__no__unlimited.*Tall Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)

## climate and dispersal (both minimal and maximal dispersal distance mixed)
stk.cd.all.fn <- grep("pure_climate_filtered__no__", summary.stk.all.fn, value = TRUE)
stk.cd.dwarf.fn <- grep("pure_climate_filtered__no__.*Dwarf Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.cd.low.fn <- grep("pure_climate_filtered__no__.*Low Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.cd.tall.fn <- grep("pure_climate_filtered__no__.*Tall Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)

## climate and biotic interactions
stk.cb.all.fn <- grep("climate_and_biointer__", summary.stk.all.fn, value = TRUE)
stk.cb.dwarf.fn <- grep("climate_and_biointer__.*Dwarf Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.cb.low.fn <- grep("climate_and_biointer__.*Low Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.cb.tall.fn <- grep("climate_and_biointer__.*Tall Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)

## climate and dispersal and biotic interactions
stk.cdb.all.fn <- grep("climate_and_biointer_filtered__", summary.stk.all.fn, value = TRUE)
stk.cdb.dwarf.fn <- grep("climate_and_biointer_filtered__.*Dwarf Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.cdb.low.fn <- grep("climate_and_biointer_filtered__.*Low Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)
stk.cdb.tall.fn <- grep("climate_and_biointer_filtered__.*Tall Shrubs.grd$", summary.stk.by.gf.fn, value = TRUE)



## make the mean of all the stack layer by layers
stk.filenames <- paste0("stk.", rep(c("c", "cd", "cb", "cdb"), each = 4), ".", rep(c("all", "dwarf", "low", 'tall'), 4), ".fn")

for(stk.fn_ in stk.filenames){
  cat("\n>", stk.fn_)
  stk.tmp_ <- NULL
  for(fn_ in get(stk.fn_)){
    cat("*")
    stk.tmp__ <- brick(file.path(ifelse(grepl("[[:punct:]]all[[:punct:]]fn$", stk.fn_), summ.stk.all.path, summ.stk.by.gf.path), fn_))
    if(is.null(stk.tmp_)) stk.tmp_ <- stk.tmp__ else stk.tmp_ <- stk.tmp_ + stk.tmp__
  }
  stk.tmp_ <- stk.tmp_ / length(get(stk.fn_))
  assign(sub(".fn$", "", stk.fn_), stk.tmp_)
}

## load couple of masks to compute stats locally
r.full.area <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_full_area_no_ice.grd")
r.from.sa <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_from_subarctic_area_no_ice.grd")
r.sa <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_subarctic_area_no_ice.grd")
r.from.la <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_from_lowarctic_area_no_ice.grd")
r.la <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_lowarctic_area_no_ice.grd")
r.ha <- raster("../data/mask_raster_arctic_area_2016-08-22/mask_higharctic_area_no_ice.grd")
mask.ids <- c('r.full.area', 'r.from.sa', 'r.sa', 'r.from.la', 'r.la', 'r.ha')

## compute the mean and quantiles of turnover and alpha div by area

# mask.id_ <- mask.ids[1]
# stk.ref.id_ <- sub(".fn$", "", stk.filenames[1])
df.out <- NULL
for(stk.ref.id_ in sub(".fn$", "", stk.filenames)){
  cat("\n>", stk.ref.id_)
  df.out_ <- NULL
  for(mask.id_ in mask.ids){
    cat("\t>", mask.id_)
    df.out__ <- NULL
    r.area_ <- get(mask.id_)
    
    stk.ref_ <- get(stk.ref.id_)
    stk.ref.names_ <- names(stk.ref_)
    stk.ref_ <- stk.ref_ * r.area_
    names(stk.ref_) <- stk.ref.names_
    r.alpha.change_ <- stk.ref_[['gain']] - stk.ref_[['lost']]
    r.turnover_ <- stk.ref_[['turnover']]
    
    ## number of non NA cells
    
    nb.cells.alpha.change <- sum(!is.na(r.alpha.change_[]))
    nb.cells.alpha.change.no.zero <- sum(r.alpha.change_[] != 0, na.rm = TRUE)
    q.alpha.change_ <- quantile(r.alpha.change_[], na.rm = TRUE)
    mean.alpha.change_ <- mean(r.alpha.change_[], na.rm = TRUE)
    sd.alpha.change_ <- sd(r.alpha.change_[], na.rm = TRUE)
    q.alpha.change.no.zero_ <- quantile(r.alpha.change_[r.alpha.change_[] != 0], na.rm = TRUE)
    mean.alpha.change.no.zero_ <- mean(r.alpha.change_[r.alpha.change_[] != 0], na.rm = TRUE)
    sd.alpha.change.no.zero_ <- sd(r.alpha.change_[r.alpha.change_[] != 0], na.rm = TRUE)
    
    
    
    nb.cells.turnover <- sum(!is.na(r.turnover_[]))
    nb.cells.turnover.no.zero <- sum(r.turnover_[] != 0, na.rm = TRUE)
    q.turnover_ <- quantile(r.turnover_[], na.rm = TRUE)
    mean.turnover_ <- mean(r.turnover_[], na.rm = TRUE)
    sd.turnover_ <- sd(r.turnover_[], na.rm = TRUE)
    q.turnover.no.zero_ <- quantile(r.turnover_[r.turnover_[] != 0], na.rm = TRUE)
    mean.turnover.no.zero_ <- mean(r.turnover_[r.turnover_[] != 0], na.rm = TRUE)
    sd.turnover.no.zero_ <- sd(r.turnover_[r.turnover_[] != 0], na.rm = TRUE)
    
    ## summup the results in a table
    stk.info_ <- unlist(strsplit(stk.ref.id_, "[[:punct:]]"))
    scenario_ <- switch(stk.info_[2],
                       c = "pure_climate",
                       cb = "climate_and_biointer",
                       cd = "pure_climate_filtered",
                       cdb = "climate_and_biointer_filtered")
    growth.form_ <- switch(stk.info_[3],
                          all = "All Shrubs",
                          dwarf = "Dwarf Shrubs",
                          low = "Low Shrubs",
                          tall = "Tall Shrubs")
    
    df.out__ <- data.frame(scenario.biomod = scenario_, area = mask.id_, growth.form = growth.form_, 
                          metric = c("alpha.div.change", "alpha.div.change", "turnover", "turnover"),
                          with.zeros = c(TRUE, FALSE, TRUE, FALSE),
                          nb.cells = c(nb.cells.alpha.change, nb.cells.alpha.change.no.zero, nb.cells.turnover, nb.cells.turnover.no.zero),
                          q0 =  c(q.alpha.change_['0%'], q.alpha.change.no.zero_['0%'], q.turnover_['0%'], q.turnover.no.zero_['0%']),
                          q25 =  c(q.alpha.change_['25%'], q.alpha.change.no.zero_['25%'], q.turnover_['25%'], q.turnover.no.zero_['25%']),
                          q50 = c(q.alpha.change_['50%'], q.alpha.change.no.zero_['50%'], q.turnover_['50%'], q.turnover.no.zero_['50%']),
                          q75 =  c(q.alpha.change_['75%'], q.alpha.change.no.zero_['75%'], q.turnover_['75%'], q.turnover.no.zero_['75%']),
                          q100 = c(q.alpha.change_['100%'], q.alpha.change.no.zero_['100%'], q.turnover_['100%'], q.turnover.no.zero_['100%']),
                          mean = c(mean.alpha.change_, mean.alpha.change.no.zero_, mean.turnover_, mean.turnover.no.zero_),
                          sd = c(sd.alpha.change_, sd.alpha.change.no.zero_, sd.turnover_, sd.turnover.no.zero_))
    df.out_ <- bind_rows(df.out_, df.out__)
  }
  df.out <- bind_rows(df.out, df.out_)
}

write.table(df.out, file = "~/Work/BRISCA/outputs/2016-08-18/SRC_baseline_statistic_table/alphadiv_change_and_turnover_stat_table_by_area.txt",
            col.names = TRUE, row.names = FALSE, sep = "\t")

df.out <- read.table("~/Work/BRISCA/outputs/2016-08-18/SRC_baseline_statistic_table/alphadiv_change_and_turnover_stat_table_by_area.txt", 
                     sep = "\t", stringsAsFactors = FALSE, header = TRUE)

df.out$area <- factor(as.character(df.out$area), levels =  c("r.full.area", "r.from.sa", "r.from.la"), labels = c("full area", "from subarctic", "from lowarctic"))
df.out$scenario.biomod <- factor(df.out$scenario.biomod, levels = c("pure_climate", "climate_and_biointer", "pure_climate_filtered", "climate_and_biointer_filtered"))

## plot the mean alpha div change by class of shrubs / by scenario


gg.dat_ <- df.out %>% 
  filter(scenario.biomod != "climate_and_biointer") %>% 
  filter(area == "full area", growth.form != 'All Shrubs') %>% 
  group_by(growth.form, scenario.biomod)

gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == TRUE, metric == 'alpha.div.change'), aes(x = growth.form, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average" ~ alpha ~ "-diversity change")) 
gg.plot
ggsave(filename = file.path(output.dir, "alpha.div.change.by.growth.form.png"), plot = gg.plot)

gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == FALSE, metric == 'alpha.div.change'), aes(x = growth.form, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average" ~ alpha ~ "-diversity change")) 
gg.plot
ggsave(filename = file.path(output.dir, "alpha.div.change.no.zero.by.growth.form.png"), plot = gg.plot)

gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == TRUE, metric == 'turnover'), aes(x = growth.form, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average turnover")) 
gg.plot
ggsave(filename = file.path(output.dir, "turnover.by.growth.form.png"), plot = gg.plot)


gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == FALSE, metric == 'turnover'), aes(x = growth.form, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average turnover")) 
gg.plot
ggsave(filename = file.path(output.dir, "turnover.no.zero.by.growth.form.png"), plot = gg.plot)


#################

gg.dat_ <- df.out %>% 
  filter(scenario.biomod != "climate_and_biointer" ) %>%
  filter(is.element(area, c("full area", "from subarctic", "from lowarctic")), growth.form == 'All Shrubs') %>% 
  group_by(area, scenario.biomod)

gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == TRUE, metric == 'alpha.div.change'), aes(x = area, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average" ~ alpha ~ "-diversity change")) 
gg.plot
ggsave(filename = file.path(output.dir, "alpha.div.change.by.area.png"), plot = gg.plot)


gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == FALSE, metric == 'alpha.div.change'), aes(x = area, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average" ~ alpha ~ "-diversity change")) 
gg.plot
ggsave(filename = file.path(output.dir, "alpha.div.change.no.zero.by.area.png"), plot = gg.plot)


gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == TRUE, metric == 'turnover'), aes(x = area, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average turnover")) 
gg.plot
ggsave(filename = file.path(output.dir, "turnover.by.area.png"), plot = gg.plot)


gg.plot <- ggplot(data = gg.dat_ %>% filter(with.zeros == FALSE, metric == 'turnover'), aes(x = area, y = mean, fill = scenario.biomod, ymin = mean - sd, ymax = mean + sd)) +
  geom_bar(stat = 'identity', position = 'dodge') + 
  # geom_errorbar(stat = 'identity', position = 'dodge') + 
  gg.theme.2 + xlab("") + 
  ylab(expression("average turnover")) 
gg.plot
ggsave(filename = file.path(output.dir, "turnover.no.zero.by.area.png"), plot = gg.plot)

