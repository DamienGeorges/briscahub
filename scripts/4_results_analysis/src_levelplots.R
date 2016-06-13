##' ---
##' @title Plot the src ouptuts for a single species
##' @descrtiption This script has been designed to produce geographical outputs
##' @author damien georges
##' @date 2016-05-11
##' @licence GPL-2
##' ---

library(rasterVis)
r1 <- raster("~/SRC_maps/src_7843_001.grd", RAT = TRUE)
r2 <- raster("~/SRC_maps/src_7844_001.grd")

r1 <- ratify(r1* r_b)
r2 <- ratify(r2* r_b)

rat <- levels(r1)[[1]]
rat$ID <- c(0, -2, 1, -1)
rat$status <- factor(c('Abs', 'Lost', 'Gain', 'Pres'), levels = c('Abs', 'Lost', 'Gain', 'Pres'))
levels(r1) <- levels(r2) <- rat
s <- stack(r2,r1) 
names(s) <- c("no dispersal", "max dispersal")

myTheme <- rasterTheme(region=c('#c2a5cf','#7b3294','#008837','#a6dba0'))
levelplot(s,  par.settings = myTheme, main = "SRC for Cassiope tetragona - climat + biotic + dispersal") + 
  layer(sp.lines(s_sa, lwd=0.8, col='black')) +
  layer(sp.lines(s_la, lwd=0.8, lty = 2, col='black')) +
  layer(sp.lines(s_ha, lwd=0.8, lty = 3, col='black')) 
# +
#   layer({
#     SpatialPolygonsRescale(layout.north.arrow(),
#                            offset = c(-4e+06,-4e+06),
#                            scale = 1000000)
#   })


## add the 
library(grid)
# grid.ls(viewport=TRUE, grobs=FALSE)  ## Prints out a listing of all viewports in plot
# grid.rect(vp = "plot_01.toplevel.vp::plot_01.legend.right.vp",
#           gp = gpar(col = "red"))
ll <- seekViewport("plot_01.legend.right.vp")
grid.text(c("___ sub-arctic\n_ _  low-arctic\n..... high-arctic"), x = 0, y = unit(1, "lines"), 
          just = c("left", "bottom"),
          gp = gpar(cex=0.8))
upViewport(ll)  
