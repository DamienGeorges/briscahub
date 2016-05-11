##' ----------------------------------------------------------------------------
##' @title reshape brisca maps
##' @description we will cut down the models projections to fit with the arctic
##'   mask from Pearson manuscript
##' @date 2019-04-22
##' @author damien g.
##' @licence GPL-2
##' ---------------------------------------------------------------------------

##' -- initialisation ---------------------------------------------------------
rm(list = ls())

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
file.id <- as.numeric(args[1])
## file.id <- 1

working.dir <- "/home/georges/BRISCA/workdir"
input.dir <- "/work/georges/BRISCA"
output.dir <- "/data/idiv_sdiv/brisca/results"

# param.file <- "/home/georges/BRISCA/biomod_framework_summary_files.txt"
param.file <- "/work/georges/BRISCA/grid_params/params_clip_output_maps.txt"
param.list <- readLines(param.file)

f_ <- param.list[file.id]
f.out_ <- sub(path.expand(input.dir), path.expand(output.dir), f_)

dir.create(dirname(f.out_), recursive = TRUE, showWarnings = FALSE)
file.copy(f_, f.out_, overwrite = TRUE, recursive = TRUE)
file.copy(sub(".grd$", ".gri", f_), sub(".grd$", ".gri", f.out_), overwrite = TRUE, recursive = TRUE)


cat("\n>", f_), "copied correctly!")
q("no")

