
library(raster)

working_dir <- "/home/georges/BRISCA/workdir"
old_mask_dir <- "/data/idiv_sdiv/brisca/results/Biomod_pure_climate_filtered/"
new_mask_dir <- " /data/idiv_sdiv/brisca/results/Present_day_masks/"

setwd(working_dir)

sp.list <- list.files(old_mask_dir)

# sp_ <- sp.list[1]

pdf("present_day_masks.pdf")
for(sp_ in sp.list){
  cat("\n>", which(sp.list == sp_), sp_)
  r_old <- raster(paste0(old_mask_dir, sp_, "/proj_pure_climat_current/individual_projections/", sp_, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData_TSSbin_filt_no_disp_invdist.grd"))
  r_new <- raster(paste0(new_mask_dir, sp_, "_present_day_mask.grd"))
  r_old_reproj <- projectRaster(r_old, r_new, method = "ngb")
  stk_ <- stack(r_new, r_old_reproj)
  names(stk_) <- c("new", "previous")
  plot(stk_)
  title(main = sp_)
}
dev.off()


###############################################
rm(list = ls())
library(raster)

working_dir <- "/home/georges/BRISCA/workdir"
mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_2017_03_09/"
pres.day.filt.dir <- "/work/georges/BRISCA/Present_day_masks_2017_03_17/"
max.disp.filt.dir <- "/work/georges/BRISCA/Future_day_masks_2017_03_17/max_dispersal/"

setwd(working_dir)

sp.list <- list.files(mod.dir)

# sp_ <- sp.list[1]
pdf("pres_fut_day_masks.pdf")
for(sp_ in sp.list){
  cat("\n>", which(sp.list == sp_), sp_)
  r_cont <- raster(file.path(mod.dir, sp_, "proj_pure_climat_current", "individual_projections", paste0(sp_, "_EMcaByTSS_mergedAlgo_mergedRun_mergedData.grd")))
  r_pres <- raster(paste0(pres.day.filt.dir, sp_, "_present_day_mask.grd"))
  r_fut <- raster(paste0(max.disp.filt.dir, sp_, "_future_day_max_disp_mask.grd"))
  stk_ <- stack(r_cont/1000, r_pres, r_fut)
  names(stk_) <- c("CA", "PresDayMask", "FutDayMask")
  plot(stk_)
  title(main = sp_)
}
dev.off()