
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
