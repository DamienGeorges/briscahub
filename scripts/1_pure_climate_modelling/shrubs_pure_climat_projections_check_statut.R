################################################################################
##' @title check that all models have been correctly projected
##' @author Damien G. 
##' @date 19/09/2015
##' @author damien.georges2 at gmail.com
##' 
##' @description 
##'   This script will check that all the models succedded
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


rm(list=ls())

## define the targeted modelling directory
# modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_invdist"
# modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_dist"
# modelling.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer"
# modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_usgs_no_flaws"
# modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_usgs_no_flaws"
# modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_strange_distrib"

# modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_2017_03_09"

modelling.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_incl_tree_2017-04-07"
# param.file <- "/work/georges/BRISCA/grid_params/params_scabp_incl_tree.txt"
param.file <- "/work/georges/BRISCA/grid_params/params_scabp_all.txt"

# modelling.dir <- "/work/georges/BRISCA/Biomod_climate_and_biointer_no_tree_2017-04-07"
# param.file <- "/work/georges/BRISCA/grid_params/params_scabp_no_tree.txt"

param.file.out <- "/work/georges/BRISCA/grid_params/params_scabp_incl_tree_missing.txt"

# ### for Pure Climate models ###
# ## read the inout parameters
# proj.tab <- read.table("/work/georges/BRISCA/grid_params/params_spcp.txt", header = FALSE, sep = " ")
# # proj.tab <- read.table("/work/georges/BRISCA/grid_params/params_csiro.txt", header = FALSE, sep = " ")
# colnames(proj.tab) <- c("job.id", "sp.name", "path.to.expl")
# head(proj.tab)
# 
# ## add columns with path to object we want to check exitance
# proj.objs <- sapply(1:nrow(proj.tab), function(i){
#   path.to.expl.var <- proj.tab$path.to.expl[i]
#   ##define the projection name
#   if(grepl("Current", path.to.expl.var)){
#     bm.proj.name <- "pure_climat_current"
#   } else{
#     bm.proj.name <- paste0("pure_climat_", sub("/", "_", sub("^.*Full_arctic_30_north/", "", path.to.expl.var))) 
#   }
#   proj.objs <- file.path(modelling.dir, 
#                         proj.tab$sp.name[i], 
#                         paste0("proj_", bm.proj.name),
#                         paste0(proj.tab$sp.name[i], ".", bm.proj.name, c(".projection.out", ".ensemble.projection.out")))
#   return(proj.objs)
# })

### for Biotic interaction models ###
## read the inout parameters
proj.tab <- read.table(param.file , header = FALSE, sep = "\t")
colnames(proj.tab) <- c("job.id", "sp.name", "path.to.clim", "path.to.biointer")
head(proj.tab)

# sp.list <- list.files(modelling.dir)
# proj.tab <- subset(proj.tab, proj.tab$sp.name %in% sp.list)

## add columns with path to object we want to check exitance
proj.objs <- sapply(1:nrow(proj.tab), function(i){
  path.to.clim.var <- proj.tab$path.to.clim[i]
  path.to.biointer.var <- proj.tab$path.to.biointer[i]
  biointer.str <- sub(".grd", "", sub("^.*_bio_inter_", "", path.to.biointer.var))
  if(grepl("Current", path.to.clim.var)){
    bm.proj.name <- paste0("pure_climat_current", biointer.str)
  } else{
    bm.proj.name <- paste0("pure_climat_", sub("/", "_", sub("^.*Full_arctic_30_north/", "", path.to.clim.var)), biointer.str) 
  }
  proj.objs <- file.path(modelling.dir, 
                         proj.tab$sp.name[i], 
                         paste0("proj_", bm.proj.name),
                         paste0(proj.tab$sp.name[i], ".", bm.proj.name, c(".projection.out", ".ensemble.projection.out")))
  return(proj.objs)
})

## reshape the proj.obj matrix
proj.objs <- t(proj.objs)
colnames(proj.objs) <- c("mod.proj.obj", "ensmod.proj.obj")

## merge it to our parameter data.frame
proj.tab <- cbind(proj.tab, proj.objs)
proj.tab$mod.proj.obj <- as.character(proj.tab$mod.proj.obj)
proj.tab$ensmod.proj.obj <- as.character(proj.tab$ensmod.proj.obj)


## check if species single and ensemble models projections have been completed
proj.tab$mod.proj.ok <- file.exists(proj.tab$mod.proj.obj)
proj.tab$ensmod.proj.ok <- file.exists(proj.tab$ensmod.proj.obj)

sum(proj.tab$mod.proj.ok)
sum(proj.tab$ensmod.proj.ok)

plot(1:nrow(proj.tab), cumsum(proj.tab$mod.proj.ok), type = 'l')
lines(1:nrow(proj.tab), 1:nrow(proj.tab), lty = 2)

## produce the new parameter file
# out.dir <- "/work/georges/BRISCA/grid_params/"
# ## produce the new parameter file
# write.table(proj.tab[ !proj.tab$ensmod.proj.ok, 2:3], file = file.path(out.dir, "params_spcp.txt"), sep = " ",
#              quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)
# dim(proj.tab[ !proj.tab$ensmod.proj.ok, 2:3])

write.table(proj.tab[ !proj.tab$mod.proj.ok, 2:4], file = param.file.out,
            sep = "\t", quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)

# which(!proj.tab$mod.proj.ok)
# which(!proj.tab$ensmod.proj.ok)


# 
# ## produce a priority subset of parmas to run (SHRUBS where Projection were successful)
# briscahub.dir <- "/home/georges/BRISCA/briscahub"
# ## -- load th species list -----------------------------------------------------
# sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
#                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
# shrub.list <- sp.tab$Biomod.name[is.element(sp.tab$Growth.form.height, c('SHRUB'))]
# ## ep.params <- proj.tab[ proj.tab$mod.proj.ok & !proj.tab$ensmod.proj.ok & is.element(as.character(proj.tab$sp.name), shrub.list), 1:4]
# 
# ep.params <- proj.tab[ proj.tab$mod.proj.ok & is.element(as.character(proj.tab$sp.name), shrub.list), 1:4]
# 
# ## ep.params <- proj.tab[ is.element(as.character(proj.tab$sp.name), shrub.list), 1:4]
# 
# write.table(ep.params, file = file.path(out.dir, "params_scabeph.txt"), sep = "\t", 
#             quote = FALSE, append = FALSE, row.names = FALSE, col.names = FALSE)



