## produce the mea summary graph of all our models ouotputs maps


rm(list = ls())

# in.dir <- "/work/georges/BRISCA/Biomod_framework_summary/summary_raster"
# out.dir <- "/work/georges/BRISCA/Biomod_framework_summary/figs"

in.dir <- "/data/idiv_sdiv/brisca/Biomod_framework_summary/summary_raster"
out.dir <- "/data/idiv_sdiv/brisca/Biomod_framework_summary/figs"


## define what type of maps we want to produce
# work.with.bin <- TRUE
# period.pattern <- "_cur" ## "_cur", "_fut" or "_diffFutCur" 

t.start <- Sys.time()
cat("\njob started on:", strftime(t.start, "%m/%d/%y - %H:%M:%S"))

## get the index we want to work with as script input
# args <- commandArgs(trailingOnly = TRUE)
# work.with.bin <- as.logical(args[1]) ## TRUE or FALSE
work.with.bin <- FALSE

cat("\n> work.with.bin:", work.with.bin)

## define the binary pattern
bin.pattern <- ifelse(work.with.bin, "_bin", "_cont") 

## -- load needed packages ----------------------------------------------------- 
#library(biomod2, lib.loc = "~/R/biomod2_pkg/biomod2_3.1-73-04")
library(raster)
#library(grid)
library(gridExtra)
library(rasterVis)

rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE) ## always overwrite existing rasters on the hard drive

mod.id <- c('pc', 'pcfno', 'pcfmax', 'cabno', 'cabmax', 'cabnofno', 'cabnofmax', 'cabmaxfno', 'cabmaxfmax')
metric.id <- c('alpha_div', 'height_mean', 'height_median', 'height_max', 'dispersal_mean')

##' Part1 var within models ----------------------------------------------------
metric.bounds.diff <- list(alpha_div = c(-50, 50), 
                           height_mean = c(-0.5, 0.5),
                           height_median = c(-0.5, 0.5),
                           height_max = c(-3, 3), 
                           dispersal_mean = c(-1, 1))

ras.theme.diff <- rasterTheme(region = rev(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac')))
## ras.theme.diff.cat <- rasterTheme(col.regions = c('#8dd3c7','#ffffb3','#bebada','#fb8072'))


##' Part2 var between models ---------------------------------------------------

nrow = ncol = length(mod.id)
nmap = nrow * ncol

## 2.1 current / future representation
for(mi in metric.id){
  cat("\n\n--- mi:", mi)
  lp.list <- vector("list", nmap)
  lp.id <- 0
  mb <- metric.bounds.diff[[mi]]
  ras.theme <- ras.theme.diff
  for(mor in mod.id){
    for(moc in mod.id){
      lp.id <- lp.id + 1
      if(mor == moc){
        cat("\n> blank plot")
        lp.list[[lp.id]] <- grid::grid.rect(gp=grid::gpar(col="white"), draw = FALSE)
      } else {
        period.pattern <- ifelse(which(mod.id == mor) > which(mod.id == mor), "_current_", "_futConsensus_")
        path.to.ras.mor <- file.path(in.dir, paste0(mor, bin.pattern, period.pattern, mi, ".grd"))
        path.to.ras.moc <- file.path(in.dir, paste0(moc, bin.pattern, period.pattern, mi, ".grd"))
        ras.sel <- raster(path.to.ras.mor) - raster(path.to.ras.moc)
        cat("\nmor:", mor, "\tmoc:", moc, "\tmi:", mi, "\tmin:", minValue(ras.sel), "\tmax:", maxValue(ras.sel) )
        lp.list[[lp.id]] <- rasterVis::levelplot(ras.sel, at = seq(mb[1], mb[2], length.out = 100), 
                                                 par.settings = ras.theme,
                                                 margin = FALSE, 
                                                 colorkey = (if(lp.id > (nmap - ncol)) list(space="bottom") else FALSE), 
                                                 main = ifelse(lp.id <= ncol, moc, ''),
                                                 ylab = ifelse(((lp.id - 1) %% ncol) == 0, mor, ''))
        
      }
    }
  }
  png(file.path(out.dir, paste0("metacompBTW_curfut_", mi, bin.pattern, ".png")), width = 300 * ncol, height = 300 * nrow, res = 80, type = "cairo")
  do.call(grid.arrange, c(lp.list, list(ncol = ncol)))
  dev.off()
}

## 2.2 diff representation
##' @note remove the median because it seems to fail
metric.id <- c('alpha_div', 'height_mean', 'height_max', 'dispersal_mean')

for(mi in metric.id){
  cat("\n\n--- mi:", mi)
  lp.list <- vector("list", nmap)
  lp.id <- 0
  mb <- metric.bounds.diff[[mi]]
  for(mor in mod.id){
    path.to.ras.mor.cur <- file.path(in.dir, paste0(mor, bin.pattern, "_current_", mi, ".grd"))
    path.to.ras.mor.fut <- file.path(in.dir, paste0(mor, bin.pattern, "_futConsensus_", mi, ".grd"))    
    ras.mor <- raster(path.to.ras.mor.fut) - raster(path.to.ras.mor.cur)
    ras.mor.bin <- ras.mor > 0
    for(moc in mod.id){
      lp.id <- lp.id + 1
      if(mor == moc){
        cat("\n> blank plot")
        lp.list[[lp.id]] <- grid::grid.rect(gp=grid::gpar(col="white"), draw = FALSE)
      } else {
        path.to.ras.moc.cur <- file.path(in.dir, paste0(moc, bin.pattern, "_current_", mi, ".grd"))
        path.to.ras.moc.fut <- file.path(in.dir, paste0(moc, bin.pattern, "_futConsensus_", mi, ".grd"))
        ras.moc <- raster(path.to.ras.moc.fut) - raster(path.to.ras.moc.cur)
        ras.moc.bin <- ras.moc > 0
        ## display teh amplitude or the accordance of differences
        if(which(mod.id == mor) > which(mod.id == moc)){
          ras.sel <- ras.mor - ras.moc
          at <- seq(mb[1], mb[2], length.out = 100)
          ras.theme <- ras.theme.diff
          col.regions <- NULL
        } else {
          ras.sel <- ratify(ras.mor.bin - (ras.moc.bin * 2))
          rat <- levels(ras.sel)[[1]]
          rat$class <- c('R+ C-', 'R+ C+', 'R- C-', 'R- C+')
          levels(ras.sel) <- rat
          at <- seq(-2, 1, 1)
          ras.theme <- NULL
          col.regions <- c('#8dd3c7','#ffffb3','#bebada','#fb8072') 
        }
        cat("\nmor:", mor, "\tmoc:", moc, "\tmi:", mi, "\tmin:", minValue(ras.sel), "\tmax:", maxValue(ras.sel) )
        lp.list[[lp.id]] <- rasterVis::levelplot(ras.sel, at = at, 
                                                 par.settings = ras.theme,
                                                 col.regions = col.regions,
                                                 margin = FALSE, 
                                                 colorkey = (if(lp.id == (nrow * (ncol - 1) + 1)) list(space="bottom") else if(lp.id == ncol) TRUE else FALSE), 
                                                 main = ifelse(lp.id <= ncol, moc, ''),
                                                 ylab = ifelse(((lp.id - 1) %% ncol) == 0, mor, ''))
        
      }
    }
  }
  png(file.path(out.dir, paste0("metacompBTW_diff_", mi, bin.pattern, ".png")), width = 300 * ncol, height = 300 * nrow, res = 80, type = "cairo")
  do.call(grid.arrange, c(lp.list, list(ncol = ncol)))
  dev.off()
}