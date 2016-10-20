##' ----------------------------------------------------------------------------
##' @title Test of maturity age modelling
##' @date 16/10/2015
##' @author Damien G.
##' @licence GPU.
##' ----------------------------------------------------------------------------

## Init the script -------------------------------------------------------------
rm(list = ls())
setwd("J:/People/Damien/BRISCA/workdir/")

library(dplyr)
library(tidyr)

## load the referency data and try selected data -------------------------------
dat.sp <- read.table("../briscahub/data/sp.list_08102015_red.txt", 
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
head(dat.sp)

dat.try <- read.csv("TRY_age_linked_traits.csv", stringsAsFactors = FALSE)

## filter try data -------------------------------------------------------------

##' @note cause some species has synonime names, we have to refer to a master file
##'   to be able to make the link btw try and our selected species names

master <- read.table("I:\\C_Write\\Signe\\aa_BRISCA\\Data\\StudySpecies\\Processed\\aa_MASTER\\MASTER13.txt", 
                     sep="\t", na.strings=c("", "NA"), header = TRUE, dec = ".", 
                     fill= TRUE, stringsAsFactors = FALSE)
master.sp <- master %>% filter(is.element(Genus.species, dat.sp$Genus.species)) %>% 
  select(Genus.species, Synonym) %>% filter(!is.na(Synonym)) %>% distinct
master.sp.names <- c(master.sp$Genus.species, master.sp$Synonym) %>% unique

## rename all the synonimus by the Genus species accepted name
dat.try.filt <- dat.try %>%
  group_by(AccSpeciesName) %>% 
  mutate(AccSpeciesName2 = ifelse(is.element(unique(AccSpeciesName), master.sp$Synonym), 
                                  master.sp$Genus.species[master.sp$Synonym == unique(AccSpeciesName)],
                                  unique(AccSpeciesName)))
## check the synonimous selected
dat.try.filt %>% select(AccSpeciesName, AccSpeciesName2) %>% filter(AccSpeciesName != AccSpeciesName2) %>% distinct

## keep only species that we are modelling
## keep only the traits that are correctly defined
dat.try.filt <- dat.try.filt %>% 
  filter(is.element(AccSpeciesName2, dat.sp$Genus.species), !is.na(TraitID), (!is.na(StdValue) | !is.na(OrigValueStr)))

## keep only species for wich we have a mig rate estimation
sp.dat.with.mig.rate <- dat.sp %>% 
  filter((!is.na(Mig.rate.m.yr)) | (!length(Mig.rate.ref))) %>%
  select(Genus.species, Biomod.name, Growthform.manual, Growth.form.height, All.height.median, Mig.rate.m.yr, Mig.rate.ref)

dat.try.filt <- dat.try.filt %>% 
  filter(is.element(AccSpeciesName2, sp.dat.with.mig.rate$Genus.species))

## make a table to summarize how many traits we have by species
dat.try.filt %>% group_by(AccSpeciesName2, TraitID) %>%
  summarise(trait.name = first(TraitName), trait.nb.obs = n())

## number of species we have data for
n_distinct(dat.try.filt$AccSpeciesName2)
dat.try.filt %>% group_by(AccSpeciesName2) %>% 
  summarise(nb.traits = n_distinct(TraitID)) %>%
  arrange(desc(nb.traits))

## number of traits we have data for
n_distinct(dat.try.filt$TraitID)
dat.try.filt %>% group_by(TraitID) %>% 
  summarise(trait.name = first(TraitName), 
            nb.species = n_distinct(AccSpeciesName2)) %>%
  arrange(desc(nb.species)) %>% data.frame

dat.try.filt %>% group_by(TraitName) %>% 
  summarise(trait.name = first(TraitID), 
            nb.species = n_distinct(AccSpeciesName2)) %>%
  arrange(desc(nb.species)) %>% data.frame


dat.try.filt.test <- dat.try.filt %>% filter((!is.na(StdValue) | !is.na(OrigValueStr))) %>% 
  select(AccSpeciesName2, TraitID, TraitName, ValueKindName, OrigValueStr, StdValue, Unit_1_UnitName) %>%
  mutate(value = ifelse(is.na(StdValue), OrigValueStr, StdValue)) %>%
  as.data.frame


dat.try.filt.test %>% group_by(TraitID) %>% 
  summarise(trait.name = first(TraitName), 
            nb.species = n_distinct(AccSpeciesName2),
            nb.values = n(),
            nb.values.levels = n_distinct(value)) %>%
  filter(nb.species >= 15) %>%
  arrange(desc(nb.species)) %>% as.data.frame


### try to select couple of traits
# TraitID                                         trait.name nb.species
# 59                                     Plant lifespan         29
# 11                           Leaf specific area (SLA)         27
# 98                             Seed storage behaviour         29
# 8                         Nitrogen fixation capacity         27
# 38                                          Woodiness         27
# 596                            Germination requirement         25
# 819      Plant resprouting capacity after disturbances         26
# 609                                  Plant propagation         24
# 679                                 Plant palatability         24
# 26                                          Seed mass         22  
# 587                                  Plant growth rate         21
# 610                                   Seed spread rate         18
# 323                            Plant height generative         16


## build a meta table with the summary value for all this traits
## we will consider the median for the continuous values and the 
## majoritaire class for factorial variable

summarise_val <- function(val){
  val.as.numeric = try(as.numeric(as.character(val)), silent = TRUE)
  if(!all(is.na(val.as.numeric))){
    return(as.character(median(val.as.numeric, na.rm = TRUE)))
  } else {
    val.count = table(na.omit(as.character(val)))
    return(names(val.count)[which.max(val.count)])
  }
}

meta.tab <- dat.try.filt.test %>% 
  filter(is.element(TraitID, c(59,11,98,8,38,596,819,609,676,26,587,610,323))) %>%
  group_by(TraitID, AccSpeciesName2) %>%
  summarize(
    trait.name = first(TraitName),
    value.summ = summarise_val(value)) %>%
  inner_join(dat.sp, by = c("AccSpeciesName2" = "Genus.species"))


head(meta.tab)
meta.tab.cast <- meta.tab %>% as.data.frame %>%
  select(-TraitID) %>%
  mutate(trait.name = gsub(" ", ".", trait.name)) %>%
  as.data.frame %>%
  spread(key = trait.name, value = value.summ)

ll <- lapply(colnames(meta.tab.cast)[25:37], function(cid){
  cat("\n\n>", cid, "------------------------------\n")
  v <- meta.tab.cast[, cid]
  if(!is.numeric(v)){
    v <- as.factor(v)
  }
  print(summary(v))
  cat("\n")
  return(v)
})

## change the variable columns to be usable

## Germination.requirement
meta.tab.cast$Germination.requirement[meta.tab.cast$Germination.requirement == "0"] <- 'No'
meta.tab.cast$Germination.requirement[meta.tab.cast$Germination.requirement == "1"] <- 'Yes'
meta.tab.cast$Germination.requirement <- as.factor(meta.tab.cast$Germination.requirement)

## Leaf.specific.area.(SLA)
meta.tab.cast[, 'Leaf.specific.area.(SLA)'] <- as.numeric(meta.tab.cast[, 'Leaf.specific.area.(SLA)'])
colnames(meta.tab.cast)[which(colnames(meta.tab.cast) == 'Leaf.specific.area.(SLA)')] <- "SLA"

## Nitrogen.fixation.capacity
meta.tab.cast$Nitrogen.fixation.capacity[meta.tab.cast$Nitrogen.fixation.capacity == "0"] <- 'No'
meta.tab.cast$Nitrogen.fixation.capacity[meta.tab.cast$Nitrogen.fixation.capacity == "N"] <- 'No'
meta.tab.cast$Nitrogen.fixation.capacity[meta.tab.cast$Nitrogen.fixation.capacity == "no"] <- 'No'
meta.tab.cast$Nitrogen.fixation.capacity <- as.factor(meta.tab.cast$Nitrogen.fixation.capacity)

## Plant.growth.rate
meta.tab.cast$Plant.growth.rate <- as.factor(meta.tab.cast$Plant.growth.rate)

## Plant.height.generative
meta.tab.cast$Plant.height.generative <- as.numeric(meta.tab.cast$Plant.height.generative)

## Plant.lifespan
meta.tab.cast$Plant.lifespan[is.element(meta.tab.cast$Plant.lifespan, c(90, 100, 120, 200, 350, 500))] <- "Long"
meta.tab.cast$Plant.lifespan[is.element(meta.tab.cast$Plant.lifespan, c(30, 48))] <- "Moderate"
meta.tab.cast$Plant.lifespan[is.element(meta.tab.cast$Plant.lifespan, c("perennial"))] <- "Perennial"
meta.tab.cast$Plant.lifespan <- as.factor(meta.tab.cast$Plant.lifespan)

## Plant.propagation
meta.tab.cast$Plant.propagation <- as.factor(meta.tab.cast$Plant.propagation)

## Plant.resprouting.capacity.after.disturbances
meta.tab.cast$Plant.resprouting.capacity.after.disturbances[meta.tab.cast$Plant.resprouting.capacity.after.disturbances == "0"] <- 'No'
meta.tab.cast$Plant.resprouting.capacity.after.disturbances <- as.factor(meta.tab.cast$Plant.resprouting.capacity.after.disturbances)

## Seed.mass
meta.tab.cast$Seed.mass <- as.numeric(meta.tab.cast$Seed.mass)

## Seed.spread.rate
meta.tab.cast$Seed.spread.rate <- as.factor(meta.tab.cast$Seed.spread.rate)

## Seed.storage.behaviour ??
## Woodiness ??


ll <- lapply(colnames(meta.tab.cast)[25:37], function(cid){
  cat("\n\n>", cid, "------------------------------\n")
  v <- meta.tab.cast[, cid]
  if(!is.numeric(v)){
    v <- as.factor(v)
  }
  print(summary(v))
  cat("\n")
  return(v)
})




## try to build some models
# f.mod1 <- biomod2::makeFormula(respName = "Mig.rate.m.yr", 
#                                explVar = meta.tab.cast[, c('Seed.Mass.Median', 'All.height.median', 'SLA')], 
#                                type = 'quadratic', 
#                                interaction.level = 0)
# ## Multiple R-squared:  0.5868,	Adjusted R-squared:  0.4262

f.mod1 <- biomod2::makeFormula(respName = "Mig.rate.m.yr", 
                               explVar = meta.tab.cast[, c('SLA'), drop = FALSE], 
                               type = 'quadratic', 
                               interaction.level = 0)



mod1 <- lm(formula = f.mod1,
           data = meta.tab.cast)

plot(y = meta.tab.cast$Mig.rate.m.yr, x = meta.tab.cast$SLA)
pred.dat <- data.frame(SLA = sort(meta.tab.cast$SLA))
lines(y = predict(mod1, newdata = pred.dat, type = 'response'), x = pred.dat$SLA, col = 'red') 





#############
#############
## EXTRACT DISPERSAL syndrom field 
#############
#############

disp.try.db <- dat.try.filt %>% filter(TraitID == 28) %>% 
  group_by(AccSpeciesName2) %>%
  sample_n(1) %>%
  mutate(Genus.species = AccSpeciesName2,
         DISP_TRY = OrigValueStr) %>% as.data.frame %>%
  select(Genus.species, DISP_TRY) %>%
  as.data.frame

write.table(disp.try.db, "disp_traits_from_try.csv", col.names = TRUE, row.names = FALSE, sep = "\t")




## -- our species data ---------------------------------------------------------

## our sp.list
dat.sp <- read.table("../briscahub/data/sp.list_08102015_red.txt", 
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)

master.sp <- master %>% filter(is.element(Genus.species, dat.sp$Genus.species)) %>% 
  select(Genus.species, Synonym) %>% filter(!is.na(Synonym)) %>% distinct

## create a list with all species names and species synonims
sp.list.full <- c(dat.sp$Genus.species, master.sp$Synonym) %>% unique
sp.list.full.short <- sapply(strsplit(sp.list.full, " "), 
                             function(x){return(paste(x[1:2], collapse = " "))})

## -- check matches with cbna database -----------------------------------------

cbna.valid <- read.table("code_cbna.csv", sep = ",", na.strings=c("", "NA"), 
                         header = TRUE, dec = ".", 
                         fill= TRUE, stringsAsFactors = FALSE)
cbna.valid$Libelle_short <- sapply(strsplit(cbna.valid$Libelle, " "), 
                                   function(x){return(paste(x[1:2], collapse = " "))})

cbna.valid.sp <- cbna.valid %>% filter(is.element(Libelle_short, sp.list.full.short))
## 60 matchs

## create a table with cbna codes for extraction
write.table(data.frame(cbna_code = cbna.valid.sp$valid_CBNA), file = "leca_db_sp_extraction_codes.csv", quote = FALSE, row.names = FALSE, col.names = FALSE)

##' Make the extraction on http://leca-bdgis.ujf-grenoble.fr/METABASE_EMABIO_V4/#

leca.traits <- read.csv("leca_db_extraction.csv", stringsAsFactor = FALSE)
leca.traits$Libelle_short <- sapply(strsplit(leca.traits$Libelle, " "), 
                                    function(x){return(paste(x[1:2], collapse = " "))})
head(leca.traits)

