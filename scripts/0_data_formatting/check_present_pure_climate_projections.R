library(raster)
library(rasterVis)

setwd("/work/georges/BRISCA/workdir")


## -- define path to models and to output directories --------------------------
# mod.dir <- "/data/idiv_sdiv/brisca/results/Biomod_pure_climate_final" ## the directory where pure climate models are stored
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_2017_03_09" ## the directory where pure climate models are stored


sp.list <- list.files(mod.dir)
proj.list <- list.files(mod.dir, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.grd", recursive = TRUE, full.names = TRUE)
save(proj.list, file = "proj.list.RData")

pdf("pure_clim_cur_proj_check.pdf")

for(sp_ in sp.list){
  cat("\n>", sp_)
  f_ <- grep(sp_, proj.list, value = TRUE)
  cat(length(f_))
  if(length(f_)){
    stk_ <- stack(f_)
    print(levelplot(stk_, main = sp_))
  }
}

dev.off()

