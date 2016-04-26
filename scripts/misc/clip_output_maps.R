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

## libraries
library("raster")
# library("parallel")
# require("maptools")
# require("ggplot2")
# require("dplyr")
# library("cartography")

# ## working paths on leca97
# working.dir <- "~/Work/BRISCA/workdir"
# input.dir <- "~/Work/BRISCA/workdir"
# output.dir <- "~/Work/BRISCA/workdir/reshaped_maps"
# path.to.shapefile <- "~/Work/BRISCA/data/cp_coast_la.shp"
# 
# file.pattern <- ".*_EM(ca|wmean)ByTSS_.*\\.grd$"
# 
# map.files.list <- list.files(input.dir, pattern = file.pattern, recursive = TRUE, full.names = TRUE) 

## working paths on sdiv clusters

## retrieve input arguments ----------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
file.id <- as.numeric(args[1])
## file.id <- 1

working.dir <- "/home/georges/BRISCA/workdir"
input.dir <- "/work/georges/BRISCA/Biomod_framework_summary"
output.dir <- "/data/idiv_sdiv/brisca/Biomod_framework_summary_reshape"
path.to.shapefile <- "/home/georges/BRISCA/data/arctic_shapefile/cp_coast_la.shp"

rasterOptions(tmpdir = "/work/georges/R_raster_georges", ## where to store raster tmp files (prevent to fill up /tmp dir)
              tmptime = 24, ## time after which raster tmp files will be deleted
              #               chunksize = 5e+08, ## size of blocks that will be written on hardrive (for I/O optimisation)
              #               maxmemory = 1e+09, ## max number of cell loaded in the memory (for I/O optimisation)
              overwrite = TRUE)

param.file <- "/home/georges/BRISCA/biomod_framework_summary_files.txt"
param.list <- readLines(param.file)

map.files.list <- param.list[file.id]

## end path definition part ----------------------------------------------------

setwd(working.dir)

##' -- read the new ref grid ---------------------------------------------------
ref.poly <- shapefile(path.to.shapefile)
ref.poly@data$WATER <- factor(ref.poly@data$WATER, levels = c(0,1,2), labels = c("water", "arctic", "sub-arctic") )
ref.poly.arctic <- ref.poly[ref.poly@data$WATER %in% c("arctic", "sub-arctic"),]

##' ggplot2 representation
# ref.poly@data$id = rownames(ref.poly@data)
# ref.poly.points = fortify(ref.poly, region="id")
# ref.poly.df = join(ref.poly.points, ref.poly@data, by="id")
# 
# ggplot(ref.poly.df) + 
#   aes(long,lat,group=group,fill = as.factor(WATER)) + 
#   geom_polygon() +
#   geom_path(color="white") +
#   coord_equal() +
#   scale_fill_manual("Ecoregion", values = c('#d0d1e6', '#fe9929', '#cc4c02'))

##' cartography representation
# opar <- par(mar = c(0,0,1.2,0))
# 

# # Layout plot
# layoutLayer(title = "Arctic area shapefile", # title of the map
#             author = "damien g.",  # no author text
#             sources = "", # no source text
#             scale = 0, # no scale
#             col = NA, # no color for the title box 
#             coltitle = "black", # color of the title
#             frame = FALSE,  # no frame around the map
#             bg = "#A6CAE0", # background of the map
#             extent = ref.poly) # set the extent of the map
# 
# # Plot the compound annual growth rate
# choroLayer(spdf = ref.poly, # SpatialPolygonsDataFrame of the regions
#            df = ref.poly@data, # data frame with compound annual growth rate
#            var = "WATER", # compound annual growth rate field in df
#            col = c('#d0d1e6', '#fe9929', '#cc4c02'), # colors 
#            breaks = c(seq(-.5,2.5,1)), # list of breaks
#            border = "grey40", # color of the polygons borders
#            lwd = 0.5, # width of the borders
#            legend.pos = "right", # position of the legend
#            legend.title.txt = "Compound Annual\nGrowth Rate", # title of the legend
#            legend.values.rnd = 2, # number of decimal in the legend values
#            add = TRUE) # add the layer to the current plot

##' -- load mops to crop -------------------------------------------------------

# ## test
# f_ <- "/home/georgeda/Work/BRISCA/workdir/pure_climate_projections/Betula.glandulosa/Betula.glandulosa_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData.grd"
# ## end test
copied.files <- lapply(map.files.list, function(f_){
  ## define the output file path and create the directory if needed 
  f.out_ <- sub(path.expand(input.dir), path.expand(output.dir), f_)
  dir.create(dirname(f.out_), recursive = TRUE, showWarnings = FALSE)
  ## load the raster to reshape
  r_ <- raster(f_)
  ## reproject the raster on shapefile system
  r_ <- projectRaster(r_, crs = crs(ref.poly.arctic), filename = f.out_, overwrite = TRUE)
  ## mask it and crop it to shapefile extent
  r_ <- crop(round(r_ * 1000), ref.poly.arctic, filename = f.out_, overwrite = TRUE)
  r_ <- mask(r_, ref.poly.arctic, filename = f.out_, overwrite = TRUE, datatype = "INT2U" )
  return(f.out_)
})

cat("\n>", unlist(copied.files), "reshaped correctly!")
q("no")

