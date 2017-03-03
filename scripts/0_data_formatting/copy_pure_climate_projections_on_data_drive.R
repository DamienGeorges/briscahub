##' ---
##' title: Copy pure climate models outputs on the Data drive
##' author damien g.
##' date: 27/02/2017
##' ---

##' ## Short description
##' 
##' We will here store the pure climate projections on the dedicated cluster drive
##' 
##' ## Script
##' 

setwd("/data/idiv_sdiv/brisca/results/Biomod_pure_climate_final/")
sp.list <- list.files("/work/georges/BRISCA/Biomod_pure_climate_strange_distrib/")
ll <- lapply(sp.list, function(.) unlink(., recursive = TRUE, force = TRUE))

in.mod.dir <- "/work/georges/BRISCA/Biomod_pure_climate_strange_distrib"
ll <- lapply(sp.list, function(.) list.files(path = file.path(in.mod.dir, .), 
                                             pattern = "EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.*gr|EMcaByTSS_mergedAlgo_mergedRun_mergedData.*gr",
                                             recursive = TRUE, full.names = TRUE))

lll <- lapply(ll, function(ll_){
  cat("\n> deal with a new species ")
  lapply(ll_, function(ll__){
    cat(".")
    out.dir_ <- sub(paste0(in.mod.dir, "/"), "", dirname(ll__))
    dir.create(out.dir_, recursive = TRUE, showWarnings = FALSE)
    file.copy(ll__, out.dir_, overwrite = TRUE)
  })
})
# 
# 
# sp.proj.files <- list.files(path = file.path(mod.dir, sp.bmname), 
#                             pattern = "EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.*grd|EMcaByTSS_mergedAlgo_mergedRun_mergedData.*grd",
#                             recursive = TRUE, full.names = TRUE)
# 
# file.copy(from = list.files("/work/georges/BRISCA/Biomod_pure_climate_strange_distrib", full.names = TRUE), to = ".")