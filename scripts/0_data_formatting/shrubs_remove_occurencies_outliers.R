##' ----------------------------------------------------------------------------
##' @title Species occurances filtering based on convex hull visualisattion
##' @date 13/10/2015
##' @author Damien G.
##' @description This script is designe to mannually remove occurences outliers that 
##'   make convex hull having a strange shape
##' @licence GPL
##' ----------------------------------------------------------------------------


library(dplyr)
##' @note cause of lack of space on hard drive I worked on my external hard drive

setwd("/media/georgeda/BackupDamien/Work/BRISCA/data/")
list.files()

##' @note all the outputs will be save in a separated directory
in.dir <- "Occurrence.tables.combined.all.sources"
out.dir <- "Occurrence.tables.combined.all.sources.no.outliers"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

## temp directory where all the raw map points will be saved
pts.out.dir <- "/media/georgeda/BackupDamien/Work/BRISCA/workdir/graph.pts"
dir.create(pts.out.dir, showWarnings = FALSE, recursive = TRUE)


##' @note we will just go species by species and remove outliers if needed

path.to.briscahub <- "~/Work/BRISCA/briscahub" ## path to teh dir where vbrscahub repos have been cloned
sp.dat <- read.table(file.path(path.to.briscahub, "data/sp.list_08102015_red.txt"), 
                     header = TRUE, sep = "\t", stringsAsFactor = FALSE)

sp.list <- unique(sp.dat$Genus.species)

## produce the dots maps of all species to detect outliers visualy
for(spe in sp.list){
  cat("\n> spe:", spe, "(", which(is.element(sp.list, spe)), ")")
  occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                        sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  occ.dat <- na.omit(occ.dat)
  png(filename = file.path(pts.out.dir, paste0(spe, ".png")))
  plot(x = occ.dat$X, y = occ.dat$Y, main = spe )
  dev.off()
}

# [1] "Abies balsamea"
spe <- "Abies balsamea"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
occ.dat <- occ.dat %>% filter(Y > -4e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [2] "Alnus incana"                    
spe <- "Alnus incana"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
occ.dat <- occ.dat %>% filter(Y > -7e+6) %>%
  filter(X < -2e+6 | X > 0 | Y < -2e+6 | Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")


# [3] "Alnus viridis"
spe <- "Alnus viridis"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

  ## remove 4 gbif points at Y > 5e+6
occ.dat <- occ.dat %>% filter(Y < 5e+6, X < 4e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [4] "Amelanchier alnifolia" => OK           
# [5] "Amelanchier bartramiana"         
spe <- "Amelanchier bartramiana"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)

## remove 4 gbif points at Y > 5e+6
occ.dat <- occ.dat %>% filter(X < -2e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [6] "Andromeda glaucophylla" => OK           
spe <- "Andromeda glaucophylla"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)

## remove 4 gbif points at Y > 5e+6
occ.dat <- occ.dat %>% filter(X < -2e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [7] "Andromeda polifolia" => OK    
# [8] "Arctostaphylos uva-ursi"
spe <- "Arctostaphylos uva-ursi"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
occ.dat <- occ.dat %>% filter(X > -6e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [9] "Arctous alpina"
spe <- "Arctous alpina"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 3 gbif points at X > 5e+6
occ.dat %>% filter(X > 5e+6)
occ.dat <- occ.dat %>% filter(X < 5e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [10] "Arctous rubra"     
spe <- "Arctous rubra"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 2 gbif points at X < -5e+6
occ.dat %>% filter(X < -5e+6)
occ.dat <- occ.dat %>% filter(X > -5e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [11] "Artemisia arctica"
spe <- "Artemisia arctica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 2 gbif points at X < -5e+6
occ.dat %>% filter(Y < 0)
## => No changes

# [12] "Artemisia canadensis" => OK
# [13] "Artemisia comata"
spe <- "Artemisia comata"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 3 gbif points at X < -3e+6 
occ.dat <- occ.dat %>% filter(X > -3e+6 | X < -4e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [14] "Artemisia dracunculus"           
spe <- "Artemisia dracunculus"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 2 gbif points at Y > 5e+6
occ.dat %>% filter(Y > 5e+6)
occ.dat <- occ.dat %>% filter(Y < 5e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [15] "Artemisia frigida"               
# [16] "Artemisia globularia"            
# [17] "Artemisia glomerata"             
# [18] "Artemisia gmelinii"
spe <- "Artemisia gmelinii"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 1 gbif points at Y < -7e+6
occ.dat %>% filter(Y < -7e+6)
occ.dat <- occ.dat %>% filter(Y > -7e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [19] "Artemisia norvegica"             
# [20] "Artemisia senjavinensis"         
# [21] "Betula fruticosa"                
# [22] "Betula glandulosa"               
spe <- "Betula glandulosa"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

occ.dat <- occ.dat %>% filter(X < 0) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [23] "Betula kenaica"                  
# [24] "Betula minor"                    
spe <- "Betula minor"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 
occ.dat <- occ.dat %>% filter(X < -3e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [25] "Betula nana"
spe <- "Betula nana"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 
## not changed

# [26] "Betula neoalaskana"
# [27] "Betula papyrifera" 
spe <- "Betula papyrifera"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove one gbif point in the middle
occ.dat %>% filter(Y > -2e+6, Y < 1e+6, X > -2e+6, X < -1e+6)
occ.dat <- occ.dat %>% filter(Y < -2e+6 | Y > 1e+6 | X < -2e+6 | X > -1e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")


# [28] "Betula pendula"                  
spe <- "Betula pendula"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 4 gbif point at the extrem bottom and extrem right
occ.dat %>% filter(X > 6e+6 | Y < -8e+6)
occ.dat <- occ.dat %>% filter(X < 6e+6 & Y > -8e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [29] "Betula platyphylla"              
spe <- "Betula platyphylla"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 8 gbif point at the extrem bottom and extrem right
occ.dat %>% filter(X > 6e+6 | Y < -8e+6)
occ.dat <- occ.dat %>% filter(X > -2e+6 & Y > -2e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [30] "Betula pubescens"                
spe <- "Betula pubescens"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## => NOT CHANGED BUT DISTRIB SUPER STRANGE

# [31] "Betula pumila"                   
# [32] "Calluna vulgaris"                
spe <- "Calluna vulgaris"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove one gbif
occ.dat <- occ.dat %>% filter(X < 6e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [33] "Caragana jubata"                 
# [34] "Cassiope lycopodioides" 
spe <- "Cassiope lycopodioides"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove one gbif
occ.dat <- occ.dat %>% filter(Y > 0) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")



# [35] "Cassiope tetragona"              
spe <- "Cassiope tetragona"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove one gbif
occ.dat <- occ.dat %>% filter(X > -6e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [36] "Chamaedaphne calyculata"         
# [37] "Chamaepericlymenum canadense"
spe <- "Chamaepericlymenum canadense"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove one gbif
occ.dat <- occ.dat %>% filter(Y > -4e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [38] "Chamaepericlymenum unalaschkense"
# [39] "Cotoneaster niger"
spe <- "Cotoneaster niger"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## => UNCHANGED

# [40] "Dasiphora fruticosa"
spe <- "Dasiphora fruticosa"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y ) 

## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(Y < 5e+6) %>% filter(Y > -5e+6 | X < 6e+6 ) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [41] "Diapensia lapponica"   
spe <- "Diapensia lapponica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$X))

## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -5e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [42] "Diapensia obovata"               
spe <- "Diapensia obovata"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$Y))

## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > 0) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [43] "Dryas ajanensis"                 
# [44] "Dryas drummondii"                
# [45] "Dryas incisa"                    
# [46] "Dryas integrifolia"              
# [47] "Dryas octopetala"                
# [48] "Dryas punctata"                  
# [49] "Empetrum atropurpureum"          
# [50] "Empetrum eamesii"                
spe <- "Empetrum eamesii"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$X))

## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X < -3.5e+6, Y < 0) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [51] "Empetrum nigrum"                 
# [52] "Gaultheria hispidula"            
spe <- "Gaultheria hispidula"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$X))

## remove 2 GBIF points
occ.dat <- occ.dat %>% filter((Y < 1e+6 & X < -3.2e+6) | (Y > 1e+6 & X > -4e+6)) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [53] "Harrimanella hypnoides"          
spe <- "Harrimanella hypnoides"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)

## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(Y < 3e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [54] "Harrimanella stelleriana"        
# [55] "Juniperus communis"              
spe <- "Juniperus communis"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)

## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(Y < 1e+6) 
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [56] "Juniperus horizontalis"          
# [57] "Kalmia polifolia"                
spe <- "Kalmia polifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$Y))
## remove 3 GBIF points
occ.dat <- occ.dat %>% filter(X > -1e+6 | X < -1.5e+6)  
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [58] "Kalmia procumbens"               
# [59] "Larix dahurica"                  
# [60] "Larix laricina"                  
# [61] "Larix sibirica"                  
spe <- "Larix sibirica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)

## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X > 0)  
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [62] "Linnaea borealis"                
# [63] "Lonicera caerulea"               
# [64] "Lonicera involucrata"            
# [65] "Lonicera villosa"                
spe <- "Lonicera villosa"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)

## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6)  
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [66] "Luetkea pectinata"               
spe <- "Luetkea pectinata"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(Y > 1.8e+6)  
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [67] "Myrica gale"                     
# [68] "Oxycoccus microcarpus"           
# [69] "Oxycoccus palustris"
spe <- "Oxycoccus palustris"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X > 0 | X < -1e+6 | Y > 0 | Y < -2e+6)  %>%
  filter(X < 3e+6 | Y > -4e+6)

write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [70] "Phyllodoce aleutica"             
# [71] "Phyllodoce caerulea"             
# [72] "Picea abies"                     
spe <- "Picea abies"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(Y > -7e+6 & Y < 7e+6)

write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [73] "Picea glauca"                    
spe <- "Picea glauca"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X < 4e+6)

write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [74] "Picea mariana"                   
spe <- "Picea mariana"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X < 4e+6)

write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [75] "Pinus pumila" => MAYBE TO RESTRICTIVE FOR THIS SPECIES                    
spe <- "Pinus pumila"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X > -1e+6 & Y > 0)

write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [76] "Pinus sibirica"                  
spe <- "Pinus sibirica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$X))
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X < 4.6e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [77] "Pinus sylvestris"                
spe <- "Pinus sylvestris"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$X))
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X < 5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")


# [78] "Populus balsamifera"             
spe <- "Populus balsamifera"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$X))
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X < 8e+6 & X > -6e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [79] "Populus suaveolens"              
# [80] "Populus tremula"                 
spe <- "Populus tremula"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$X))
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [81] "Populus tremuloides"             
spe <- "Populus tremuloides"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$X))
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X > -7e+6 & (X < 2e+6 | Y < -2e+6))
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [82] "Prunus padus"                    
spe <- "Prunus padus"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$X))
## remove 2 GBIF points
occ.dat <- occ.dat %>% filter(X > -5e+6 & (X < 2e+6 | Y < -2e+6))
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [83] "Prunus pensylvanica"
spe <- "Prunus pensylvanica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6 | X > -1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [84] "Rhododendron aureum"             
spe <- "Rhododendron aureum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [85] "Rhododendron groenlandicum"      
spe <- "Rhododendron groenlandicum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y < 5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [86] "Rhododendron lapponicum"         
spe <- "Rhododendron lapponicum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -4e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [87] "Rhododendron tomentosum"
spe <- "Rhododendron tomentosum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y)

# [88] "Ribes glandulosum"               
spe <- "Ribes glandulosum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$Y))
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -3e+6 & (X < -2e+6 | X > -1e+6 | Y > 0))
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [89] "Ribes hirtellum"                 
spe <- "Ribes hirtellum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [90] "Ribes hudsonianum"               
spe <- "Ribes hudsonianum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [91] "Ribes lacustre"                  
# [92] "Ribes nigrum"                    
spe <- "Ribes nigrum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y < 5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [93] "Ribes oxyacanthoides"            
# [94] "Ribes spicatum"                  
spe <- "Ribes spicatum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y < 7e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [95] "Ribes triste"                    
spe <- "Ribes triste"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6 | Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [96] "Rosa acicularis"                 
spe <- "Rosa acicularis"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$Y))
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -3.5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [97] "Rosa majalis"                    
# [98] "Rubus acaulis"
spe <- "Rubus acaulis"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
plot(x = occ.dat$X, y = occ.dat$Y)
occ.dat <- occ.dat %>% filter(X > -5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [99] "Rubus arcticus"
spe <- "Rubus arcticus"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y < -1e+6 | Y > 1e+6 | X < -1e+6 | X > 1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [100] "Rubus chamaemorus"               
spe <- "Rubus chamaemorus"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [101] "Rubus idaeus"                    
spe <- "Rubus idaeus"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -7e+6 & X < 7e+6 & Y > -7e+6 & Y < 7e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [102] "Rubus pubescens"                 
spe <- "Rubus pubescens"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [103] "Rubus stellatus"                 
spe <- "Rubus stellatus"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [104] "Salix acutifolia"                
# [105] "Salix alaxensis"                 
# [106] "Salix arbuscula"                 
spe <- "Salix arbuscula"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [107] "Salix arbusculoides"             
# [108] "Salix arctica"                   
spe <- "Salix arctica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 4e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [109] "Salix arctophila"                
spe <- "Salix arctophila"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [110] "Salix argyrocarpa"               
# [111] "Salix ballii"                    
# [112] "Salix barclayi"                  
spe <- "Salix barclayi"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [113] "Salix barrattiana"               
spe <- "Salix barrattiana"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -4.5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [114] "Salix bebbiana"                  
spe <- "Salix bebbiana"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 0 | Y < -1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [115] "Salix berberifolia"              
spe <- "Salix berberifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 3e+6 & Y < 3.1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [116] "Salix boganidensis"              
# [117] "Salix brachycarpa"               
spe <- "Salix brachycarpa"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6 | Y > 1.5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [118] "Salix burjatica"                 
spe <- "Salix burjatica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [119] "Salix calcicola"                 
spe <- "Salix calcicola"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$X))
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [120] "Salix candida"                   
spe <- "Salix candida"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$X))
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [121] "Salix caprea"                    
spe <- "Salix caprea"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -7e+6 & X < 7e+6 & Y > -7e+6 & Y < 7e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [122] "Salix chamissonis"               
spe <- "Salix chamissonis"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [123] "Salix cordata"                   
spe <- "Salix cordata"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -7e+6 & X < 7e+6 & Y > -7e+6 & Y < 7e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [124] "Salix fuscescens"                
spe <- "Salix fuscescens"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [125] "Salix glauca"                    
# [126] "Salix hastata"                   
# [127] "Salix herbacea"                  
spe <- "Salix herbacea"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y < 2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [128] "Salix interior"                  
spe <- "Salix interior"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -4e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [129] "Salix krylovii"                  
# [130] "Salix lanata"                    
spe <- "Salix lanata"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -4e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [131] "Salix lapponum"                  
spe <- "Salix lapponum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [132] "Salix myrsinifolia"              
spe <- "Salix myrsinifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [133] "Salix myrsinites"                
# [134] "Salix myrtillifolia"             
spe <- "Salix myrtillifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6 | Y > 1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [135] "Salix myrtilloides"              
# [136] "Salix niphoclada"                
spe <- "Salix niphoclada"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -1.5e+6 | Y > 1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [137] "Salix nummularia"                
# [138] "Salix ovalifolia"                
spe <- "Salix ovalifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [139] "Salix pedicellaris"              
spe <- "Salix pedicellaris"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$X), 20)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [140] "Salix pellita"                   
spe <- "Salix pellita"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [141] "Salix pentandra"                 
# [142] "Salix phlebophylla"              
spe <- "Salix phlebophylla"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y < 3.5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [143] "Salix phylicifolia"              
# [144] "Salix planifolia"                
# [145] "Salix polaris"                   
# [146] "Salix pseudomonticola"           
# [147] "Salix pseudomyrsinites"          
spe <- "Salix pseudomyrsinites"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -1.5e+6 | Y > 1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [148] "Salix pulchra"                   
# [149] "Salix pyrolifolia"               
# [150] "Salix recurvigemmis"             
# [151] "Salix reptans"                   
# [152] "Salix reticulata"                
# [153] "Salix richardsonii"
spe <- "Salix richardsonii"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -3.5e+6) %>%
  filter(X < -1.5e+6 | X > 0 | Y < 0 | Y > 1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [154] "Salix rosmarinifolia"            
spe <- "Salix rosmarinifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6, X < 6e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [155] "Salix rotundifolia"              
spe <- "Salix rotundifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -4e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [156] "Salix saxatilis"                 
# [157] "Salix serissima"                 
spe <- "Salix serissima"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [158] "Salix sphenophylla"              
spe <- "Salix sphenophylla"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [159] "Salix triandra"                  
spe <- "Salix triandra"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -6e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [160] "Salix tschuktschorum"            
# [161] "Salix tyrrellii"                 
spe <- "Salix tyrrellii"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -1e+6, Y < 1.5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [162] "Salix udensis"                   
spe <- "Salix udensis"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6, X < 5.5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [163] "Salix uva-ursi"                  
# [164] "Salix vestita"                   
spe <- "Salix vestita"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -3e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [165] "Salix viminalis"                 
# [166] "Sambucus racemosa"               
spe <- "Sambucus racemosa"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
head(sort(occ.dat$X))
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -6.1e+6) %>%
  filter(X < -2e+6 | X > 0 | Y < -2e+6 | Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [167] "Shepherdia canadensis"           
spe <- "Shepherdia canadensis"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6 | Y > 1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [168] "Sibbaldia procumbens"            
spe <- "Sibbaldia procumbens"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6 | Y > 1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [169] "Sorbus aucuparia"                
spe <- "Sorbus aucuparia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 6e+6, Y < 5e+6) %>%
  filter(X < 0 | X > 1.5e+6 | Y < 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [170] "Sorbus groenlandica"             
spe <- "Sorbus groenlandica"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -3e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [171] "Sorbus sambucifolia"             
spe <- "Sorbus sambucifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [172] "Sorbus scopulina"                
spe <- "Sorbus scopulina"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [173] "Spiraea betulifolia"             
spe <- "Spiraea betulifolia"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -4e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [174] "Spiraea media"                   
spe <- "Spiraea media"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [175] "Spiraea salicifolia"             
# [176] "Spiraea stevenii"                
spe <- "Spiraea stevenii"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -3e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")


# [177] "Swida alba"                      
# [178] "Swida sericea"                   
spe <- "Swida sericea"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y < 5e+6, X < 2e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [179] "Therorhodion glandulosum"        
spe <- "Therorhodion glandulosum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [180] "Thymus praecox"                  
spe <- "Thymus praecox"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -8e+6, Y < 0, X < 5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [181] "Thymus serpyllum"                
spe <- "Thymus serpyllum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < 3e+6) %>%
  filter(X < -2e+6 | Y < -1e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [182] "Vaccinium caespitosum"
spe <- "Vaccinium caespitosum"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -3e+6 | Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")


# [183] "Vaccinium myrtilloides"          
# [184] "Vaccinium myrtillus"             
# [185] "Vaccinium ovalifolium"           
spe <- "Vaccinium ovalifolium"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
tail(sort(occ.dat$X[occ.dat$Y < 0]))
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -3.1e+6 | Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [186] "Vaccinium uliginosum"            
# [187] "Vaccinium vitis-idaea"           
spe <- "Vaccinium vitis-idaea"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X > -6e+6,  Y < 5e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [188] "Veronica fruticans"              
spe <- "Veronica fruticans"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(Y > -7e+6)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")

# [189] "Viburnum edule" 
spe <- "Viburnum edule"
occ.dat <- read.table(file.path(in.dir, paste0(spe, ".txt")),
                      sep = "\t", header = TRUE, stringsAsFactors = FALSE)
occ.dat <- na.omit(occ.dat)
## remove 1 GBIF points
occ.dat <- occ.dat %>% filter(X < -2e+6 | Y > 0)
write.table(occ.dat, file.path(out.dir, paste0(spe, ".txt")), row.names = FALSE, col.names = TRUE, sep = "\t")




## merge the modified and unmodified version of occ data -----------------------

out.dir.merged <- paste0(out.dir, '.merged')
unlink(out.dir.merged)
dir.create(out.dir.merged, showWarnings = FALSE, recursive = TRUE)
options(warn = 2)

for(spe in sp.list){
  if(file.exists(file.path(out.dir, paste0(spe, ".txt")))){ ## file have been modified
    file.copy(file.path(out.dir, paste0(spe, ".txt")),
              file.path(out.dir.merged, paste0(spe, ".txt")),
              overwrite = TRUE)
  } else {
    file.copy(file.path(in.dir, paste0(spe, ".txt")),
              file.path(out.dir.merged, paste0(spe, ".txt")),
              overwrite = TRUE)
  }
}

## reconstruct the dots graphs -------------------------------------------------

pts.out.dir.merged <- paste0(pts.out.dir, ".merged")
unlink(pts.out.dir.merged)
dir.create(pts.out.dir.merged, showWarnings = FALSE, recursive = TRUE)
options(warn = 2)

for(spe in sp.list){
  cat("\n> spe:", spe, "(", which(is.element(sp.list, spe)), ")")
  occ.dat <- read.table(file.path(out.dir.merged, paste0(spe, ".txt")),
                        sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  occ.dat <- na.omit(occ.dat)
  png(filename = file.path(pts.out.dir.merged, paste0(spe, ".png")))
  plot(x = occ.dat$X, y = occ.dat$Y, main = spe )
  dev.off()
}
