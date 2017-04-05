##' ---
##' title: script to check that we are happy with our new biotic interaction masks
##' date: 07/03/2017
##' author: Damien g.
##' ---

##' @note: this is just a diagnose plot.. not much effort have been done for the presentation
##' 

library(raster)
library(rasterVis)

setwd("/work/georges/BRISCA/workdir")

in.dir.biotic <- "/work/georges/BRISCA/Biomod_biotic_interaction_maps_2017-03-20"
in.dir.pres.day.mask <- "/work/georges/BRISCA/Present_day_masks_2017_03_17"



bif.list <- list.files(in.dir.biotic, ".grd$", full.names = TRUE)
sp.list <- sort(unique(sub("_.*$", "", basename(bif.list))))

pdf("check_biotic_interaction_maps.pdf")

# sp_ <- sp.list[116]
for(sp_ in sp.list){
  cat("\n>", sp_ , "(", which(sp.list == sp_), "/", length(sp.list), ")")
    bi.stk.files <- grep(sp_, bif.list, value = TRUE)
  bi.no.disp <- grep("no_dipersal", bi.stk.files, value = TRUE)
  bi.max.disp <- grep("max_dipersal", bi.stk.files, value = TRUE)
  bi.unlimited.disp <- grep("unlimited_dipersal", bi.stk.files, value = TRUE)
  
  ras.no.disp.cur <- stack(bi.no.disp, bands = 1)
  ras.no.disp.fut <- stack(bi.no.disp, bands = 2)
  ras.max.disp.fut <- stack(bi.max.disp, bands = 2)
  ras.unlimited.disp.fut <- stack(bi.unlimited.disp, bands = 2)
  
  ras.pres.day.mask <- raster(file.path(in.dir.pres.day.mask, paste0(sp_, "_present_day_mask.grd")))
  
  max.no.disp.cur <- max(maxValue(ras.no.disp.cur), 1)
  
  stk <- stack(ras.pres.day.mask, ras.no.disp.cur / max.no.disp.cur, 
               ras.no.disp.fut / max.no.disp.cur, ras.max.disp.fut / max.no.disp.cur,
               ras.unlimited.disp.fut / max.no.disp.cur)
  names(stk) <- c("present day", "bioInt cur", "bioInt fut no", "bioInt fut max", "bioInt fut ulim")
  print(levelplot(stk, main = sp_, sub = paste0("max curent BioInt: ", round(max.no.disp.cur))))
}
dev.off()


