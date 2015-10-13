##' ----------------------------------------------------------------------------
##' @title Convex hull computation function
##' @date 13/10/2015
##' @author Gilles. D.
##' @note This function is based on the IUCN Convexe Hull calculation extracted and
##'   addapted by Gilles.
##' @description The aim of this function is to build the full convex hull for a
##'   set of species.  
##' @licence Ask Gilles D. before reuse.
##' ----------------------------------------------------------------------------


##########################################
###### Arguments of the function:  ######
##########################################
### 'DATA' is the input file. Should be a dataframe with occurrences as rows 
### and the three first colums giving respectively the latitude, longitude 
### and name of the species. Lat and Long should be given in decimal degrees

### 'country_map' is a option if you have to have 
### the countries boundaries as a background for the mapping. 
### The file should be opened before using the function. See above for africa shapefiles

### 'Cell_size' you should give the cell width used

### 'DrawMap' logical (TRUE or FALSE). If you want to have pdf file created with a map for each species
### showing the occurrences and AOO. By default, it is TRUE

### 'add.legend' logical (TRUE or FALSE). If you want to add a legend indicating the statistics
### of each species in a corner of the map. By default, it is TRUE

### 'export_shp' logical if you want to get the shapefile of the convex hull of each species.  By default, it is TRUE

### 'file_name' character, name given to the exported pdf, if not given name is 'IUCN'

IUCN_eval <- function (DATA, country_map=NULL, Cell_size, DrawMap=T, add.legend=T, file_name=NULL, export_shp=TRUE, Filter_Date=F, Year_upper=NULL) {
  if(!any(names(installed.packages()[,1])=="proj4")) install.packages("proj4")
  if(!any(names(installed.packages()[,1])=="grDevices")) install.packages("grDevices")
#   if(!any(names(installed.packages()[,1])=="geometry")) install.packages("geometry")
  if(!any(names(installed.packages()[,1])=="sp")) install.packages("sp")
  if(!any(names(installed.packages()[,1])=="rgdal")) install.packages("rgdal")
  if(!any(names(installed.packages()[,1])=="rgeos")) install.packages("rgeos")
  if(!any(names(installed.packages()[,1])=="raster")) install.packages("raster")

  library(proj4)
  library(grDevices)
#   library(geometry)
  library(sp)
  library(rgdal)
  library(rgeos)
  library(raster)
  #### Fonction for determining the UTM zone of the points
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }
#   #### Fonction for drawing Hull convex
#   if(DrawMap){
#     Plot_ConvexHull <- function(xcoord, ycoord, lcolor, LWD=1){
#       hpts <- chull(x = xcoord, y = ycoord)
#       hpts <- c(hpts, hpts[1])
#       lines(xcoord[hpts], ycoord[hpts], col = lcolor, lwd= LWD)
#     }
#   }
#
  Convex_Hull_Poly <- function(XY){
    hpts <- chull(x =  XY[,1], y = XY[,2]); hpts <- c(hpts, hpts[1])
    POLY <- "POLYGON(("
    for (i in 1:length(hpts)){
      POLY <- paste(POLY,XY[hpts[i],2]," ",XY[hpts[i],1], sep="")
      if(i!=length(hpts)) POLY <- paste(POLY,", ", sep="")
      if(i==length(hpts)) POLY <- paste(POLY,"))", sep="")
    }
    p1 = readWKT(POLY)  
    return(p1)
  }

  areaPolygons<- function(spPoly, proj4string = NULL) {
    if(class(spPoly)[[1]] != "SpatialPolygonsDataFrame"&
         class(spPoly)[[1]] != "SpatialPolygons") {
      stop("spPoly must be a SpatialPolygonsDataFrame or a
           SpatialPolygons object.")
    }
    require(sp)
    require(rgdal)
    if(!is.null(proj4string)) {
      if(class(proj4string)[[1]] != "CRS") {
        stop("The proj4string must be of class CRS")
      }
      spP<- spTransform(spPoly, CRS = CRS(projUTM))
    }
    else {
      spP<- spPoly
    }
    areas<- unlist(lapply(spP@polygons, function(x) a<- x@area))
    return(areas)
    }

  Nsp <- length(unique(DATA[,3])) ### Number of species in the input file
  
  Results <- as.data.frame(matrix(NA,6,Nsp))
  colnames(Results) <- as.character(unique(DATA[,3]))
  rownames(Results) <- c("EOO","AOO", "Nbe_occ", "Nbe_loc", "Category CriteriaB", "Category code")
  Results_Full <- list()

  ConvexHulls_poly <- list()

  if(Filter_Date) {DATA_Filtered <- DATA[which(DATA[,4]>Year_upper),]
  DATA_Full <- list(DATA, DATA_Filtered)
  }else{DATA_Full <- list(DATA)}

  for (j in 1:length(DATA_Full)){
  if(DrawMap) {
    if(!is.null(file_name)) {NAME <- file_name}else{NAME <- "IUCN"}
    if(j==1) pdf(paste(NAME,".pdf", sep=""), width=20, height=20)
    if(j==2) pdf(paste(NAME,"_filtered_dataset",".pdf", sep=""), width=20, height=20)
  }
  for (i in 1:Nsp){
    XY <- cbind(DATA_Full[[j]][which(DATA_Full[[j]][,3]==unique(DATA_Full[[j]][,3])[i]),2],DATA_Full[[j]][which(DATA_Full[[j]][,3]==unique(DATA_Full[[j]][,3])[i]),1])
    p1 <- Convex_Hull_Poly(cbind(XY[,2],XY[,1]))
    crs(p1) <- "+proj=longlat +datum=WGS84"
    ConvexHulls_poly[[i]] <- p1; names(ConvexHulls_poly)[[i]] <- as.character(unique(DATA_Full[[j]][,3])[i])
    ##### The UTM zone of the median of the longitude is determined
    UTM_ZONE <- long2UTM(median(XY[,1]))
    #### The UTM coordinates are obtained trough a projection
    projUTM <- paste("+proj=utm +zone=",UTM_ZONE," +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep="")
    coordUTM <- matrix(unlist(proj4::project(XY,proj=projUTM,inverse =FALSE)), ncol=2)
    EOO <- round(areaPolygons(p1, proj4string = CRS(projUTM))/1000000,1)
    
    
#     XY_shp <- as.data.frame(XY)
#     coordinates(XY_shp) = c(1,2)
#     crs(XY_shp) <- crs(NationalParks)
#     Link <- over(NationalParks, XY_shp, returnList=T)
#     which(Link)
#     Link[[]][]
#     lapply(Link, )
    
    
    #### Convex hull computation
    #     CONV <- convhulln(coordUTM, options="Fa")
    #     EOO <- CONV$vol/1000000  ### EOO
    AOO <- nrow(unique(floor(coordUTM/Cell_size)))*Cell_size*Cell_size  ### AOO
    Results["EOO",i] <- EOO; Results["AOO",i] <- AOO
    Results["Nbe_occ",i] <- nrow(XY); Results["Nbe_loc",i] <- nrow(unique(floor(coordUTM/Cell_size)))
    Nbe_Loc <- nrow(unique(floor(coordUTM/Cell_size)))
    ### Criteria B assessment
    if(EOO<20000 || AOO<2000) {
      if(EOO<20000){
        Rank_EOO <- 3
        if(EOO<5000){
          Rank_EOO <- 2
          if(EOO<100){
            Rank_EOO <- 1
            
          }}}else(Rank_EOO <- 4)
      if(AOO<2000){
        Rank_AOO <- 3
        if(AOO<500){
          Rank_AOO <- 2
          if(AOO>10){
            Rank_AOO <-1
          }}}else{Rank_AOO <- 4}
      if(Nbe_Loc<=10){
        Rank_Loc <-3
        if(Nbe_Loc <=5){
          Rank_Loc <-2
          if(Nbe_Loc==1){
            Rank_Loc <- 1
          }}}else{Rank_Loc <- 4}
        }else{Rank_EOO <- Rank_Loc <- Rank_AOO <- 4}

    Rank_B1a <- max(Rank_EOO, Rank_Loc)
    Rank_B2a <- max(Rank_AOO, Rank_Loc)
    Rank_CriteriaB <- min(Rank_B1a, Rank_B2a)
    if(Rank_CriteriaB==1) Cat <- "CE"
    if(Rank_CriteriaB==2) Cat <- "E"
    if(Rank_CriteriaB==3) Cat <- "Vu"
    if(Rank_CriteriaB>3) Cat <- "NT or LC"
    if(Rank_B1a>Rank_B2a) Cat_Code <- paste(Cat,"B2a")
    if(Rank_B1a<Rank_B2a) Cat_Code <- paste(Cat,"B1a")
    if(Rank_B1a==Rank_B2a) Cat_Code <- paste(Cat,"B1a+B2a")
    Results["Category CriteriaB",i] <- Cat
    Results["Category code",i] <- Cat_Code

    par(las=1, mar=c(10,10,10,6))
    if(DrawMap) {
      par(mar=c(5, 4, 4, 2), xpd=F, las=1)
      if(add.legend) nf <- layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), c(4,1.5), c(4,1.5))
      if(!is.null(country_map)) plot(country_map, axes=FALSE, lty=1,border="black", xlim=c(range(XY[,1])), ylim=c(range(XY[,2])), lwd=1) #
      if(is.null(country_map)) plot(p1,  axes=FALSE, xlim=c(range(XY[,1])), ylim=c(range(XY[,2])), xlab="", ylab="")
      if(!is.null(country_map)) plot(p1, add=T)
      axis(1, outer=F, cex.axis=3)  #pos=min(range(XY[,2]))-2)
      axis(2, outer=F, cex.axis=3)  #pos=min(range(XY[,2]))-2)
      box()
      points(XY[,1],XY[,2], pch=19, cex=3, col="grey")
      #       Plot_ConvexHull(xcoord = XY[,1], ycoord = XY[,2], lcolor = "blue", LWD=2)
      mtext(unique(DATA_Full[[j]][,3])[i], side=3, cex=3)
      if(add.legend) {
        par(mar=c(1,1,1,1), xpd=T)
        plot(1:10, 1:10, type="n", bty='n', xaxt='n', yaxt='n')
        legend(1,10,  c(paste("EOO=", round(EOO,1),"km2"),
                        paste("AOO=", format(AOO, scientific = 5),"km2"),
                        paste("Number of occurrences=", nrow(XY)),
                        paste("Number of localities=",nrow(unique(floor(coordUTM/Cell_size)))),
        paste("IUCN category according to criteria B", Cat)), cex=3,bg = grey(0.9) )}
    }
    
  }
  if(DrawMap)dev.off()
  Results_Full[[j]] <- Results; names(Results_Full)[j] <- ifelse(j==1, "Unfiltered_dataset", "Filtered_dataset")
}
  if(export_shp){list(Results, ConvexHulls_poly)}else{Results}
}




