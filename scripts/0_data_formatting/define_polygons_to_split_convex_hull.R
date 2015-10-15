## define polygons to split Convex hull by hand

library(raster)
r <- raster("~/Work/BRISCA/workdir/pure_climate_projections/Betula.glandulosa/Betula.glandulosa_EMwmeanByTSS_mergedAlgo_mergedRun_mergedData_TSSbin.grd")

x11()
## define the eurassia polygon
plot(r)
ext.eurasia <- drawExtent()
r.ext.eurasia <- crop(r,ext.eurasia)
extent(r.ext.eurasia) <- ext.eurasia
plot(r.ext.eurasia)
poly.eurasia <- drawPoly()

## define the north america polygon
plot(r)
ext.north.america <- drawExtent()
r.north.america <- crop(r,ext.north.america)
extent(r.north.america) <- ext.north.america
plot(r.north.america)
poly.north.america <- drawPoly()

## define the high north polygon
plot(r)
ext.high.north <- drawExtent()
r.high.north <- crop(r,ext.high.north)
extent(r.high.north) <- ext.high.north
plot(r.high.north)
poly.high.north <- drawPoly()

## close the x11 device
dev.off()

## save the convex-hull splitting polygons
save(poly.eurasia, poly.north.america, poly.high.north, file = "~/Work/BRISCA/briscahub/data/convex_hull_splitting_poly.RData")

## check the defined polygons
library(rasterVis)
## create a mask independent from species selected
r[!is.na(r[])] <- 0
## create a plot that illustrate the selected areas
png("~/Work/BRISCA/briscahub/data/convex_hull_splitting_area.png")
levelplot(r, margin = FALSE, colorkey = FALSE) +
  layer(sp.lines(poly.eurasia, lwd = 0.8, col = 'red')) +
  layer(sp.lines(poly.north.america, lwd = 0.8, col = 'blue')) +
  layer(sp.lines(poly.high.north, lwd = 0.8, col = 'green'))
dev.off()  
