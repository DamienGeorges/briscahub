################################################################################
##' @title produce pure cllimate modelling summary graphs
##'
##' @author Damien G. 
##' @date 12/10/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   The aim of this script is to build some visual tools to help us to get an
##'   idea of how accurate our models are.
##'   
##' @log
##' 
##' @licencing GPL
##'     Copyright (C) 2015  Damien G.
##' 
##'     This program is free software: you can redistribute it and/or modify
##'     it under the terms of the GNU General Public License as published by
##'     the Free Software Foundation, either version 3 of the License, or
##'     (at your option) any later version.
##' 
##'     This program is distributed in the hope that it will be useful,
##'     but WITHOUT ANY WARRANTY; without even the implied warranty of
##'     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##'     GNU General Public License for more details.
##' 
##'     You should have received a copy of the GNU General Public License
##'     along with this program.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

## -- init the script ----------------------------------------------------------
rm(list = ls())
setwd("/work/georges/BRISCA/")

## retrieve input arguments ----------------------------------------------------
# args <- commandArgs(trailingOnly = TRUE)
# sp.id <- as.numeric(args[1])

bm.scores.all <- NULL

for(sp.id in 1:189){
  cat("\n> sp:", sp.id, "/189 ---------------------------------------------\n")
  
  ## -- load needed packages ----------------------------------------------------- 
  library(biomod2, lib.loc = "~/R/biomod2_pkg/biomod2_3.1-73-04")
  
  ## -- define path to models and to output directories --------------------------
  mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
  out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final_summary"
  dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)
  briscahub.dir <- "/home/georges/BRISCA/briscahub"
  
  ## -- load th species list -----------------------------------------------------
  sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                       sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  
  ## -- select a species e want to work with -------------------------------------
  sp.name <- sp.tab$Genus.species[sp.id]
  sp.name.bm <- sp.tab$Biomod.name[sp.id]
  
  ## -- get the models score -----------------------------------------------------
  ## load single models
  bm.mod.name <- load(file.path(mod.dir, sp.name.bm, 
                                paste0(sp.name.bm, ".pure_climat.models.out")))
  bm.mod <- get(bm.mod.name)
  rm(list = bm.mod.name)
  ## get single models scores
  bm.mod.scores <- get_evaluations(bm.mod, as.data.frame = TRUE)
  ## load ensemble models
  bm.ensmod.name <- load(file.path(mod.dir, sp.name.bm, 
                                   paste0(sp.name.bm, ".pure_climatensemble.models.out")))
  bm.ensmod <- get(bm.ensmod.name)
  rm(list = bm.ensmod.name)
  ## get ensemble models scores
  bm.ensmod.scores <- get_evaluations(bm.ensmod, as.data.frame = TRUE)
  
  ## keep only the needed part of the data and merge thems
  bm.scores <- rbind(bm.mod.scores, bm.ensmod.scores)[, c("Model.name", "Eval.metric", "Testing.data")]
  ## add a column to be able to regroup models by algo and by species
  bm.scores$Model.algo <- sub("_.*$", "", bm.scores$Model.name)
  bm.scores$Genus.species <- sp.name
  bm.scores$Biomod.name <- sp.name.bm
  ## store results in a meta table
  bm.scores.all <- rbind(bm.scores.all, bm.scores)
}

save(bm.scores.all, file = "/work/georges/BRISCA/Biomod_pure_climate_final_summary/bm.scores.all.RData")

library(plyr)
## remove all NAs
bm.scores.all <- na.omit(bm.scores.all)

## produce the graph of score by species and by algo ---------------------------
## compute the median, and quantiles 2.5% and 97.5% by species and algo
bm.scores.all.summ <- ddply(bm.scores.all, .(Genus.species, Biomod.name, 
                                             Model.algo, Eval.metric),
                            summarize,
                            med.score = quantile(Testing.data, .5),
                            q0025.score = quantile(Testing.data, .025),
                            q0975.score = quantile(Testing.data, .975))
head(bm.scores.all.summ)
## reshape the table to be able to produce plots easily
bm.scores.all.summ <- ddply(bm.scores.all.summ, .(Genus.species, Biomod.name, 
                                                  Model.algo),
                            summarize,
                            TSS.med = med.score[Eval.metric == 'TSS'],
                            TSS.q0025 = q0025.score[Eval.metric == 'TSS'],
                            TSS.q0975 = q0975.score[Eval.metric == 'TSS'],
                            ROC.med = med.score[Eval.metric == 'ROC'],
                            ROC.q0025 = q0025.score[Eval.metric == 'ROC'],
                            ROC.q0975 = q0975.score[Eval.metric == 'ROC'])
head(bm.scores.all.summ)

gg <- ggplot(data = bm.scores.all.summ, aes(x = ROC.med, y = TSS.med, xmin = ROC.q0025, xmax = ROC.q0975, ymin = TSS.q0025, ymax = TSS.q0975, colour = Model.algo)) +
  geom_errorbar(alpha = .2) + geom_errorbarh(alpha = .2) + 
  geom_point() +
  xlab('ROC') + ylab('TSS') + coord_cartesian(xlim = c(0.5, 1), ylim = c(0,1)) +
  ggtitle('Pure climate models scores\n by species and by algo')
ggsave(filename = "/work/georges/BRISCA/Biomod_pure_climate_final_summary/spcm_scores_scatter_by_algo_by_species.png",
       plot = gg, width = 10, height = 5)

## produce the graph of score by species --------------------------------------
## compute the median, and quantiles 2.5% and 97.5% by species and algo
bm.scores.by.species <-   bm.scores.all.summ <- ddply(bm.scores.all, .(Genus.species, Biomod.name, 
                                               Eval.metric),
                              summarize,
                              med.score = quantile(Testing.data, .5),
                              q0025.score = quantile(Testing.data, .025),
                              q0975.score = quantile(Testing.data, .975))
head(bm.scores.by.species)
## reshape the table to be able to produce plots easily
bm.scores.by.species <- ddply(bm.scores.by.species, .(Genus.species, Biomod.name),
                            summarize,
                            TSS.med = med.score[Eval.metric == 'TSS'],
                            TSS.q0025 = q0025.score[Eval.metric == 'TSS'],
                            TSS.q0975 = q0975.score[Eval.metric == 'TSS'],
                            ROC.med = med.score[Eval.metric == 'ROC'],
                            ROC.q0025 = q0025.score[Eval.metric == 'ROC'],
                            ROC.q0975 = q0975.score[Eval.metric == 'ROC'])

gg <- ggplot(data = bm.scores.by.species, aes(x = ROC.med, y = TSS.med, xmin = ROC.q0025, xmax = ROC.q0975, ymin = TSS.q0025, ymax = TSS.q0975)) +
  geom_errorbar(alpha = .2) + geom_errorbarh(alpha = .2) + 
  geom_point() +
  xlab('ROC') + ylab('TSS') + coord_cartesian(xlim = c(0.5, 1), ylim = c(0,1)) +
  ggtitle('Pure climate models scores\n by species')
ggsave(filename = "/work/georges/BRISCA/Biomod_pure_climate_final_summary/spcm_scores_scatter_by_species.png",
       plot = gg, width = 10, height = 5)

## produce the boxplots of score by species ------------------------------------
gg <- ggplot(data = bm.scores.all, aes(y = Testing.data, x = Genus.species)) +
  geom_boxplot() + facet_grid(~Eval.metric, scales = 'free') +
  ylab('Models scores') + xlab('Species') + coord_flip() + 
  ggtitle('Pure climate models scores\n by species')
ggsave(filename = "/work/georges/BRISCA/Biomod_pure_climate_final_summary/spcm_scores_boxplot_by_species.png",
       plot = gg, width = 10, height = 30)

## produce the boxplots of score by algo ------------------------------------
gg <- ggplot(data = bm.scores.all, aes(y = Testing.data, x = Model.algo)) +
  geom_boxplot() + facet_grid(~Eval.metric, scales = 'free') +
  ylab('Models scores') + xlab('Algorithm') + coord_flip() + 
  ggtitle('Pure climate models scores\n by algo')
ggsave(filename = "/work/georges/BRISCA/Biomod_pure_climate_final_summary/spcm_scores_boxplot_by_algo.png",
       plot = gg, width = 10, height = 5)

## produce the density plots of score by algo ----------------------------------
gg <- ggplot(data = bm.scores.all, aes(x = Testing.data, colour = Model.algo)) +
  geom_density() + facet_grid(~Eval.metric, scales = 'free') +
  xlab('Models scores') + ylab('density') +
  ggtitle('Pure climate models scores\n by algo')
ggsave(filename = "/work/georges/BRISCA/Biomod_pure_climate_final_summary/spcm_scores_density_by_algo.png",
       plot = gg, width = 10, height = 5)


