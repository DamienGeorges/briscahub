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

modelling.dir <- "/work/georges/BRISCA/Biomod_pure_climate_wm"
proj.tab <- read.table("/work/georges/BRISCA/grid_params/params_spcp.txt", header = FALSE, sep = " ")
# proj.tab <- read.table("~/Work/BRISCA/grid_params/params_spcp.txt", header = FALSE, sep = " ")
colnames(proj.tab) <- c("job.id", "sp.name", "path.to.expl")
head(proj.tab)

## add columns with path to object we want to check exitance
proj.objs <- sapply(1:nrow(proj.tab), function(i){
  path.to.expl.var <- proj.tab$path.to.expl[i]
  ##define the projection name
  if(grepl("Current", path.to.expl.var)){
    bm.proj.name <- "pure_climat_current"
  } else{
    bm.proj.name <- paste0("pure_climat_", sub("/", "_", sub("^.*Full_arctic_30_north/", "", path.to.expl.var))) 
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

## produce the new parameter file
out.dir <- "/work/georges/BRISCA/grid_params/"
write.table(proj.tab[ !proj.tab$mod.proj.ok, 2:3], file = file.path(out.dir, "params_spcp20G.txt"), sep = " ", 
            quote = FALSE, append = FALSE, row.names = TRUE, col.names = FALSE)

