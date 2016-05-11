## produce the mea summary graph of all our models ouotputs maps


rm(list = ls())

in.dir <- "/work/georges/BRISCA/Biomod_framework_summary/summary_raster"
out.dir <- "/work/georges/BRISCA/Biomod_framework_summary/figs"


## define what type of maps we want to produce
# work.with.bin <- TRUE
# period.pattern <- "_cur" ## "_cur", "_fut" or "_diffFutCur" 

t.start <- Sys.time()
cat("\njob started on:", strftime(t.start, "%m/%d/%y - %H:%M:%S"))

## get the index we want to work with as script input
args <- commandArgs(trailingOnly = TRUE)
work.with.bin <- as.logical(args[1]) ## TRUE or FALSE
period.pattern <- as.character(args[2]) ## "_cur", "_fut" or "_diffFutCur" 

cat("\n> work.with.bin:", work.with.bin)
cat("\n> period.pattern:", period.pattern)

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
metric.bounds.cont <- list(alpha_div = c(0, 100), 
                           height_mean = c(0, 0.8),
                           height_median = c(0, 0.2),
                           height_max = c(-3, 3), 
                           dispersal_mean = c(-1, 1))

ras.theme.diff <- rasterTheme(region = rev(c('#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac')))
ras.theme.cont <- rasterTheme(region = (c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')))

nrow = length(mod.id)
ncol = length(metric.id)
nmap = nrow * ncol
lp.list <- vector("list", nmap)
lp.id <- 0
for(mo in mod.id){
  for(mi in metric.id){
#     mo <- mod.id[1]
#     mi <- metric.id[1]
    lp.id <- lp.id + 1
    path.to.ras.cur <- file.path(in.dir, paste0(mo, bin.pattern, "_current_", mi, ".grd"))
    path.to.ras.fut <- file.path(in.dir, paste0(mo, bin.pattern, "_futConsensus_", mi, ".grd"))
    ## select the appropriate raster we are interested in
    if(period.pattern == "_cur"){
      ras.sel <- raster(path.to.ras.cur)
      mb <- metric.bounds.cont[[mi]]
      ras.theme <- ras.theme.cont
    } else if(period.pattern == "_fut"){
      ras.sel <- raster(path.to.ras.fut)
      mb <- metric.bounds.cont[[mi]]
      ras.theme <- ras.theme.cont
    } else if(period.pattern == "_diffFutCur"){
      ras.sel <- raster(path.to.ras.fut) - raster(path.to.ras.cur)
      mb <- metric.bounds.diff[[mi]]
      ras.theme <- ras.theme.diff
    } else stop("unknown period.pattern arg.")
    cat("\nmo:", mo, "\tmi:", mi, "\tmin:", minValue(ras.sel), "\tmax:", maxValue(ras.sel) )
    lp.list[[lp.id]] <- rasterVis::levelplot(ras.sel, at = seq(mb[1], mb[2], length.out = 100), 
                                             par.settings = ras.theme,
                                             margin = FALSE, 
                                             colorkey = (if(lp.id > (nmap - ncol)) list(space="bottom") else FALSE), 
                                             main = ifelse(lp.id <= ncol, mi, ''),
                                             ylab = ifelse(((lp.id - 1) %% ncol) == 0, mo, ''))
  }
}

png(file.path(out.dir, paste0("metacomp", period.pattern, bin.pattern, ".png")), width = 300 * ncol, height = 300 * nrow, res = 80, type = "cairo")
do.call(grid.arrange, c(lp.list, list(ncol = ncol)))
dev.off()



