##' @title check traits values availabilites for BRISCA project in TTT
##' @date 28-04-2016
##' @author damien g.

library(raster)
library(dplyr)

briscahub.dir <- "~/Work/BRISCA/briscahub"
shrubhub.dir <- "~/Work/SHRUBS/ShrubHub"

## load trait data
(load(file.path(shrubhub.dir, "workspace/traits/try_ttt_clean.RData")))
head(try.ttt.clean)

## load study area buffer
area.poly <- shapefile(file.path(briscahub.dir, 'data/Buffer500Km/Buffer500Km.shp'))
plot(area.poly)

## load species list
sp.tab <- read.table(file.path(briscahub.dir, "data/sp.list_08102015_red.txt"),
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)
sp.tab <- sp.tab[ sp.tab$Growth.form.height == 'SHRUB', ]
sp.tab$Genus <- sub(" .*$", "", sp.tab$Genus.species)
## nb of species
length(unique(sp.tab$Genus.species))
## nb of genus
length(unique(sp.tab$Genus))

## load master file to check for synonyms
master <- read.table("~/Work/BRISCA/data/MASTER14.txt", header = TRUE, stringsAsFactors = FALSE, sep="\t", na.strings=c("", "NA"))
synoym.tab <- master %>% select(Genus.species, Synonym) %>% na.omit %>% filter(Genus.species %in% sp.tab$Genus.species)

## check what we should extract within trait table

## deal with synonyms
for(s.id_ in 1:nrow(synoym.tab)){
  try.ttt.clean[try.ttt.clean$AccSpeciesName == synoym.tab[s.id_, "Synonym"], "AccSpeciesName"] <- synoym.tab[s.id_, "Genus.species"]
}
## update genus name
try.ttt.clean$Genus <-  sub(" .*$", "", try.ttt.clean$AccSpeciesName)

## number of species represented in ttt
try.ttt.clean %>% filter(AccSpeciesName %in% sp.tab$Genus.species) %>% summarise(n.species = length(unique(AccSpeciesName)))
## associated number of genus cover
try.ttt.clean %>% filter(AccSpeciesName %in% sp.tab$Genus.species) %>% summarise(n.genus = length(unique(Genus)))

## full number of genus (without considering species level)
try.ttt.clean %>% filter(Genus %in% sp.tab$Genus) %>% summarise(n.genus = length(unique(Genus)))

## keep only geolocated height traits -----------------------------------------------------

try.ttt.clean.height <- try.ttt.clean %>% filter( TraitShort == 'PlantHeight',
                                                 !is.na(Lat), !is.na(Lon))

try.ttt.clean.height %>% filter(AccSpeciesName %in% sp.tab$Genus.species) %>% summarise(n.species = length(unique(AccSpeciesName)))
## associated number of genus cover
try.ttt.clean.height %>% filter(AccSpeciesName %in% sp.tab$Genus.species) %>% summarise(n.genus = length(unique(Genus)))

## full number of genus (without considering species level)
try.ttt.clean.height %>% filter(Genus %in% sp.tab$Genus) %>% summarise(n.genus = length(unique(Genus)))

## get the number of points by species
sp.to.keep <- try.ttt.clean.height %>% filter(AccSpeciesName %in% sp.tab$Genus.species) %>% group_by(AccSpeciesName) %>% 
  summarise(n.height.mesure = n()) %>% ungroup %>% arrange(desc(n.height.mesure)) %>% as.data.frame %>% filter(n.height.mesure >= 25) %>% as.data.frame

## 

r.bio1 <- raster('/media/georgeda/equipes/emabio/GIS_DATA/Global/PHYSIQUE/CLIMATE/Current/Wordclim/30s/bio/bio_1')
r.bio12 <- raster('/media/georgeda/equipes/emabio/GIS_DATA/Global/PHYSIQUE/CLIMATE/Current/Wordclim/30s/bio/bio_12')


extr.bio1 <- extract(r.bio1, try.ttt.clean.height %>% select(Lon,Lat) %>% as.data.frame)
extr.bio12 <- extract(r.bio12, try.ttt.clean.height %>% select(Lon,Lat) %>% as.data.frame)

try.ttt.clean.height <- cbind(try.ttt.clean.height, data.frame(bio1 = extr.bio1)) 
try.ttt.clean.height <- cbind(try.ttt.clean.height, data.frame(bio12 = extr.bio12)) 
try.ttt.clean.height.filt <- try.ttt.clean.height %>% filter(AccSpeciesName %in% sp.to.keep$AccSpeciesName)

library(ggplot2)
ggplot(try.ttt.clean.height.filt) +
  aes(y = StdValue, x = bio1, col = as.factor(AccSpeciesName)) +
  geom_point() + geom_smooth(method = 'lm')

ggplot(try.ttt.clean.height.filt) +
  aes(y = StdValue, x = bio1) +
  geom_point() + geom_smooth(method = 'lm') + facet_wrap(~AccSpeciesName, scales = 'free_y')

ggplot(try.ttt.clean.height.filt) +
  aes(y = StdValue, x = bio12) +
  geom_point() + geom_smooth(method = 'lm') + facet_wrap(~AccSpeciesName, scales = 'free_y')
