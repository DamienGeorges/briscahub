### HEADER #####################################################################
##' @title Clean brisca sdm project data directory
##'
##' @author Damien G.
##' @contact damien.georges2 at gmail.com
##' 
##' @date 09/09/2015
##' 
##' @description 
##'   The aim of this script is to remove all files we will not need for the
##'   species distribution modelling part of our project (i.e 2020 and 2050
##'   projections, duplicated data, ...). The lighter version of the data should 
##'   then be push on our calculation servers. 
##'   
##' @note 
##'   The original version of the data have been proceed by Anne O.B. and stored
##'   in Signe network backup. Directories structure have been concerved.
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
## END OF HEADER ###############################################################

## Start of script --------------------------------------------------------
rm(list = ls())

##' @note the current version of the data are stored on an external harddrive

## here we will remove all useless files from future climate projections
path.to.dir <- "/media/georgeda/Seagate Expansion Drive/Aarhus/aa_BRISCA/Data/Climate/Macroclimate/Future/CIAT_AR5_bio_prec_tmean_tmax_tmin/Processed/Projected_polar_laea_10km/Full_arctic_30_north"

file.list <- list.files(path.to.dir, recursive = TRUE, full.names = TRUE, include.dirs = FALSE)
file.to.remove <- grep("bioproj_multi.gr*|ddeg", file.list, value = TRUE, invert = TRUE)
unlink(file.to.remove)

## here files have been removed but empty directory remains, we should try to remove some
file.list <- list.dirs(path.to.dir, recursive = TRUE, full.names = TRUE)
file.to.remove <- grep("tave[[:digit:]]+$", file.list, value = TRUE)
unlink(file.to.remove, force = TRUE, recursive = TRUE)

## remove prec, tave and tave10_asc directories
file.list <- list.dirs(path.to.dir, recursive = TRUE, full.names = TRUE)
file.to.remove <- grep("tave$|tave10_asc$|info$|prec$", file.list, value = TRUE)
unlink(file.to.remove, force = TRUE, recursive = TRUE)

## now we should have a minimal version of wanted data
file.list <- list.dirs(path.to.dir, recursive = TRUE, full.names = TRUE)

## End of script ----------------------------------------------------------

