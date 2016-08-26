##' ---
##' @title Plot the src ouptuts for the shrub species
##' @descrtiption This script has been designed to produce geographical outputs
##' @author damien georges
##' @date 2016-05-11
##' @licence GPL-2
##' ---

rm(list = ls())

## set some parameters
same.baseline <- TRUE ## do we consider the same baseline (climate filtered no dispersal) as a baseline or 
## each scenario current prediction as baseline
machine <- "leca97" # "sdiv" ## the name of the machine the script will run on
n.cores <- 10 ## number of resuired cores

## define the main paths to data
if(machine == "leca97"){
  briscahub.dir <- "~/Work/BRISCA/briscahub/" ## on leca97
  src.maps.path <-  paste0("~/Work/BRISCA/workdir/_SRC/", ifelse(same.baseline, "SRC_baseline_maps", "SRC_maps")) ## on leca97
  summ.stk.path <- paste0("~/Work/BRISCA/outputs/2016-08-18/", ifelse(same.baseline, "SRC_baseline", "SRC"), "_alpha_and_turnover_stack") ## on leca97
  param.tab.path <- "~/Work/BRISCA/workdir/_SRC/params_src.txt"
  out.dir.path <- "~/Work/BRISCA/figures/2016-08-18/alphadiv_change_and_turnover_maps"
}
dir.create(out.dir.path, showWarnings = FALSE, recursive = TRUE)

library(rasterVis)
library(maptools)

s_buff <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/Buffer.shp"))
s_sa <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/Sub_Arctic.shp"))
s_la <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/Low_Arctic.shp"))
s_ha <- shapefile(file.path(briscahub.dir, "data/Arctic_buffers/High_Arctic.shp"))
s_coast <- shapefile("/home/georgeda/Work/BRISCA/data/cp_coast_la.shp")
## coastine need to be rotated
s_coast2 <- elide(s_coast, rotate = 180, center = c(0,0))
crs(s_coast2) <- crs(s_buff)
s_ice <- shapefile("/home/georgeda/Work/BRISCA/data/Perm-ice/permaice_glaciers.shp")
## ice layer needs to be rotated
s_ice2 <- elide(s_ice, rotate = 180, center = c(0,0))
crs(s_ice2) <- crs(s_buff)

s_coast2 <- crop(s_coast2, s_buff)
s_ice2 <- crop(s_ice2, s_buff)

summary.stk.fn <- list.files(summ.stk.path, ".grd$")

##' ## Turnover and alpha diversity changes (rcp and gcm merged)

## climate filenames
stk.c.fn <- grep("pure_climate__no__unlimited", summary.stk.fn, value = TRUE)
## climate and dispersal (both minimal and maximal dispersal distance mixed)
stk.cd.fn <- grep("pure_climate_filtered__no__", summary.stk.fn, value = TRUE)
## climate and biotic interactions
stk.cb.fn <- grep("climate_and_biointer__", summary.stk.fn, value = TRUE)
## climate and dispersal and biotic interactions
stk.cdb.fn <- grep("climate_and_biointer_filtered__", summary.stk.fn, value = TRUE)

length(stk.c.fn) + length(stk.cd.fn) + length(stk.cb.fn) + length(stk.cdb.fn)


## make the mean of all the stack layer by layers
stk.c <- NULL
for(fn_ in stk.c.fn){
  cat("*")
  stk.tmp_ <- brick(file.path(summ.stk.path, fn_))
  if(is.null(stk.c)) stk.c <- stk.tmp_ else stk.c <- stk.c + stk.tmp_
}
stk.c <- stk.c / length(stk.c.fn)

stk.cd <- NULL
for(fn_ in stk.cd.fn){
  cat("*")
  stk.tmp_ <- brick(file.path(summ.stk.path, fn_))
  if(is.null(stk.cd)) stk.cd <- stk.tmp_ else stk.cd <- stk.cd + stk.tmp_
}
stk.cd <- stk.cd / length(stk.cd.fn) 

stk.cb <- NULL
for(fn_ in stk.cb.fn){
  cat("*")
  stk.tmp_ <- brick(file.path(summ.stk.path, fn_))
  if(is.null(stk.cb)) stk.cb <- stk.tmp_ else stk.cb <- stk.cb + stk.tmp_
}
stk.cb <- stk.cb / length(stk.cb.fn)

stk.cdb <- NULL
for(fn_ in stk.cdb.fn){
  cat("*")
  stk.tmp_ <- brick(file.path(summ.stk.path, fn_))
  if(is.null(stk.cdb)) stk.cdb <- stk.tmp_ else stk.cdb <- stk.cdb + stk.tmp_
}
stk.cdb <- stk.cdb / length(stk.cdb.fn)

##' group the results in stacks

stk.turnover <- brick(stk.c[['turnover']], stk.cd[['turnover']], stk.cb[['turnover']], stk.cdb[['turnover']])
names(stk.turnover) <- c("climate", "climate_dispersal", "climate_biointer", "climate_dispersal_biointer")


## change alpha diversity into alpha diversity change
stk.alphadiv.change <-  brick(stk.c[['gain']] - stk.c[['lost']],
                              stk.cd[['gain']] - stk.cd[['lost']],
                              stk.cb[['gain']] - stk.cb[['lost']],
                              stk.cdb[['gain']] - stk.cdb[['lost']])
names(stk.alphadiv.change) <- c("climate", "climate_dispersal", "climate_biointer", "climate_dispersal_biointer")


q.turnover <- quantile(stk.turnover, c(seq(0, 0.1, by = .01), seq(0.9, 1, by = .01)))
q.alphadiv.change <- quantile(stk.alphadiv.change, c(seq(0, 0.1, by = .01), seq(0.9, 1, by = .01)))

## rescale the out of bound values to the min and max of the plotting scale
stk.turnover <- reclassify(stk.turnover, c(2.5, Inf, 2.5))
stk.alphadiv.change <- reclassify(stk.alphadiv.change, c(-Inf, -50, -50, 
                                                  50, Inf, 50))
names(stk.turnover) <- names(stk.alphadiv.change) <- c("climate", "climate_dispersal", "climate_biointer", "climate_dispersal_biointer")

## define a custom color theme for the raster
ras.theme.div <- rasterTheme(region = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026'))
ras.at.div <- seq(0, 2.5, length.out = 100)

ras.theme.div.change <- rasterTheme(region = c(rep('#b2182b',2^5),rep('#d6604d',2^4),rep('#f4a582',2^3),rep('#fddbc7',2^2),'#f7f7f7',rep('#d1e5f0',2^2),rep('#92c5de',2^3),rep('#4393c3',2^4),rep('#2166ac',2^5)))
# color.pal.div.change <- colorRampPalette(c(rep('#b2182b',2*5),rep('#d6604d',2*4),rep('#f4a582',2*3),rep('#fddbc7',2*2),'#f7f7f7',rep('#d1e5f0',2*2),rep('#92c5de',2*3),rep('#4393c3',2*4),rep('#2166ac',2*5)), interpolate = 'spline')
# ras.theme.div.change <- rasterTheme(region = color.pal.div.change(100))
ras.at.div.change <- seq(-50, 50, length.out = 100)

lp.turnover <- levelplot(subset(stk.turnover, c("climate", "climate_dispersal", "climate_dispersal_biointer")), 
                         layout=c(3, 1), main = expression("Turnover"), par.settings = ras.theme.div, at = ras.at.div,
                         zscaleLog = FALSE) +
  layer(sp.lines(s_coast2[s_coast2@data$WATER == 0, ], lwd=0.8, col='darkgrey')) +
  layer(sp.lines(s_buff, lwd=0.8, col='darkgrey')) +
  layer(sp.lines(s_ice2, lwd=0.8, col = NA, fill = 'white')) +
  layer(sp.lines(s_sa, lwd=0.8, col='black')) +
  layer(sp.lines(s_la, lwd=0.8, lty = 2, col='black')) +
  layer(sp.lines(s_ha, lwd=0.8, lty = 3, col='black'))

lp.alphadiv.change <- levelplot(subset(stk.alphadiv.change, c("climate", "climate_dispersal", "climate_dispersal_biointer")), 
                         layout=c(3, 1), main = expression(alpha ~ "-diversity change"),
                         par.settings = ras.theme.div.change, at = ras.at.div.change,
                         zscaleLog = FALSE) +
  layer(sp.lines(s_coast2[s_coast2@data$WATER == 0, ], lwd=0.8, col='darkgrey')) +
  layer(sp.lines(s_buff, lwd=0.8, col='darkgrey')) +
  layer(sp.lines(s_ice2, lwd=0.8, col = NA, fill = 'white')) +
  layer(sp.lines(s_sa, lwd=0.8, col='black')) +
  layer(sp.lines(s_la, lwd=0.8, lty = 2, col='black')) +
  layer(sp.lines(s_ha, lwd=0.8, lty = 3, col='black')) 

png(file.path(out.dir.path, "turnover_and_alphadiv_change_vs_scenario.png"), width = 1024, height = 768)
print(lp.turnover, split=c(1, 1, 1, 2), more=TRUE)
print(lp.alphadiv.change, split=c(1, 2, 1, 2))
dev.off()

##' ## second graph

stk.turnover <- brick(stk.c[['turnover']], stk.cd[['turnover']], stk.cb[['turnover']], stk.cdb[['turnover']])
stk.gain <- brick(stk.c[['gain']], stk.cd[['gain']], stk.cb[['gain']], stk.cdb[['gain']])
stk.lost <- brick(stk.c[['lost']], stk.cd[['lost']], stk.cb[['lost']], stk.cdb[['lost']])
stk.alphadiv.change <-  brick(stk.c[['gain']] - stk.c[['lost']],
                              stk.cd[['gain']] - stk.cd[['lost']],
                              stk.cb[['gain']] - stk.cb[['lost']],
                              stk.cdb[['gain']] - stk.cdb[['lost']])
names(stk.turnover) <- names(stk.gain) <- names(stk.lost) <- names(stk.alphadiv.change) <- c("climate", "climate_dispersal", "climate_biointer", "climate_dispersal_biointer")


library(dplyr)
library(ggplot2)

tab.alphadiv.diff <- data.frame(metric = "alpha-diversity change", do.call(rbind, lapply(names(stk.alphadiv.change), 
                                                                                         function(n_){
                                                                                           ## get the number of pixels that have a certain amount of species
                                                                                           df_ <- data.frame(table(round(stk.alphadiv.change[[n_]][])))
                                                                                           colnames(df_) <- c('nb.species', 'nb.pixels')
                                                                                           df_$nb.species <- as.numeric(as.character(df_$nb.species))
                                                                                           df_$nb.pixels <- as.numeric(as.character(df_$nb.pixels))
                                                                                           ## fill the gaps
                                                                                           min.val_ <- round(min(minValue(stk.alphadiv.change)))
                                                                                           max.val_ <- round(max(maxValue(stk.alphadiv.change)))
                                                                                           df__ <- data.frame(nb.species = min.val_:max.val_, nb.pixels = 0) %>% 
                                                                                             filter(!is.element(nb.species, df_$nb.species))
                                                                                           bind_cols(data.frame(scenario = n_, bind_rows(df_, df__)))
                                                                                         } )))
tab.turnover <- data.frame(metric = "turnover", do.call(rbind, lapply(names(stk.turnover), 
                                                                      function(n_){
                                                                        ## get the number of pixels that have a certain amount of species
                                                                        df_ <- data.frame(table(round(stk.turnover[[n_]][])))
                                                                        colnames(df_) <- c('nb.species', 'nb.pixels')
                                                                        df_$nb.species <- as.numeric(as.character(df_$nb.species))
                                                                        df_$nb.pixels <- as.numeric(as.character(df_$nb.pixels))
                                                                        ## fill the gaps
                                                                        min.val_ <- round(min(minValue(stk.turnover)))
                                                                        max.val_ <- round(max(maxValue(stk.turnover)))
                                                                        df__ <- data.frame(nb.species = min.val_:max.val_, nb.pixels = 0) %>% 
                                                                          filter(!is.element(nb.species, df_$nb.species))
                                                                        bind_cols(data.frame(scenario = n_, bind_rows(df_, df__)))
                                                                      } )))
tab.gain <- data.frame(metric = "gain", do.call(rbind, lapply(names(stk.gain), 
                                                              function(n_){
                                                                ## get the number of pixels that have a certain amount of species
                                                                df_ <- data.frame(table(round(stk.gain[[n_]][])))
                                                                colnames(df_) <- c('nb.species', 'nb.pixels')
                                                                df_$nb.species <- as.numeric(as.character(df_$nb.species))
                                                                df_$nb.pixels <- as.numeric(as.character(df_$nb.pixels))
                                                                ## fill the gaps
                                                                min.val_ <- round(min(minValue(stk.gain)))
                                                                max.val_ <- round(max(maxValue(stk.gain)))
                                                                df__ <- data.frame(nb.species = min.val_:max.val_, nb.pixels = 0) %>% 
                                                                  filter(!is.element(nb.species, df_$nb.species))
                                                                bind_cols(data.frame(scenario = n_, bind_rows(df_, df__)))
                                                              } )))
tab.lost <- data.frame(metric = "lost", do.call(rbind, lapply(names(stk.lost), 
                                                              function(n_){
                                                                ## get the number of pixels that have a certain amount of species
                                                                df_ <- data.frame(table(round(stk.lost[[n_]][])))
                                                                colnames(df_) <- c('nb.species', 'nb.pixels')
                                                                df_$nb.species <- as.numeric(as.character(df_$nb.species))
                                                                df_$nb.pixels <- as.numeric(as.character(df_$nb.pixels))
                                                                ## fill the gaps
                                                                min.val_ <- round(min(minValue(stk.lost)))
                                                                max.val_ <- round(max(maxValue(stk.lost)))
                                                                df__ <- data.frame(nb.species = min.val_:max.val_, nb.pixels = 0) %>% 
                                                                  filter(!is.element(nb.species, df_$nb.species))
                                                                bind_cols(data.frame(scenario = n_, bind_rows(df_, df__)))
                                                              } )))

tab.pix.count <- rbind(tab.alphadiv.diff, tab.turnover, tab.gain, tab.lost)
# colnames(tab.pix.count) <- c('metric', 'scenario', 'nb.species', 'nb.pixels')
# tab.pix.count$nb.species <- as.numeric(as.character(tab.pix.count$nb.species))
# tab.pix.count$nb.pixels <- as.numeric(as.character(tab.pix.count$nb.pixels))

## define an article fig friendly theme
gg.theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                  panel.grid.major = element_line(colour = "grey90"),
                  panel.grid.minor = element_blank(), #element_line(colour = "grey80"),
                  panel.border = element_rect(fill = NA, colour = 'grey80'),
                  panel.background = element_rect(fill = NA),
                  strip.background = element_rect(fill = NA, colour = 'grey80'),
                  legend.background = element_blank()) 


gg.pix.count <- ggplot(data = tab.pix.count %>% filter(scenario != 'climate_biointer'), aes(x = nb.species, y = nb.pixels, col = scenario, fill = scenario)) + ## , pch = metric
  facet_wrap(~ metric, scales = "free_x")   #+ coord_cartesian(ylim = c(0, max(tab.pix.count$nb.pixels)))
  # geom_point() +
  # geom_line() + 
  # geom_density(stat = "identity", alpha = .3) +  
  # geom_smooth() +
gg.pix.count + geom_point(alpha = .8) + gg.theme

gg.pix.count.bp <-  ggplot(data = tab.pix.count %>% filter(scenario != 'climate_biointer') %>% mutate(nb.species.cut = cut(nb.species, breaks = seq(-40, 60, by = 5))),
       aes(x = nb.species.cut, y = nb.pixels, col = scenario)) + geom_boxplot() +  facet_wrap(~ metric, scales = "free_x")
gg.pix.count.bp + gg.theme

ggsave(file.path(out.dir.path, "pixel_counts_graphs_density.png"), plot = gg.pix.count + geom_density(stat = "identity", alpha = .3) + gg.theme)
ggsave(file.path(out.dir.path, "pixel_counts_graphs_points.png"), plot = gg.pix.count + geom_point(alpha = .8) + gg.theme)
ggsave(file.path(out.dir.path, "pixel_counts_graphs_bp.png"), plot = gg.pix.count.bp + gg.theme)


##### removing zeros

gg.pix.count <- ggplot(data = tab.pix.count %>% filter(scenario != 'climate_biointer', nb.species != 0), aes(x = nb.species, y = nb.pixels, col = scenario, fill = scenario)) + ## , pch = metric
  facet_wrap(~ metric, scales = "free_x")   #+ coord_cartesian(ylim = c(0, max(tab.pix.count$nb.pixels)))
# geom_point() +
# geom_line() + 
# geom_density(stat = "identity", alpha = .3) +  
# geom_smooth() +
gg.pix.count + geom_point(alpha = .8) + gg.theme

gg.pix.count.bp <-  ggplot(data = tab.pix.count %>% filter(scenario != 'climate_biointer', nb.species != 0) %>% mutate(nb.species.cut = cut(nb.species, breaks = seq(-40, 60, by = 5))),
                           aes(x = nb.species.cut, y = nb.pixels, col = scenario)) + geom_boxplot() +  facet_wrap(~ metric, scales = "free_x")
gg.pix.count.bp + gg.theme

ggsave(file.path(out.dir.path, "pixel_counts_graphs_density_no_zero.png"), plot = gg.pix.count + geom_density(stat = "identity", alpha = .3) + gg.theme)
ggsave(file.path(out.dir.path, "pixel_counts_graphs_points_no_zero.png"), plot = gg.pix.count + geom_point(alpha = .8) + gg.theme)
ggsave(file.path(out.dir.path, "pixel_counts_graphs_bp_no_zero.png"), plot = gg.pix.count.bp + gg.theme)


#######################
