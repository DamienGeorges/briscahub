##' ----------------------------------------------------------------------------
##' @title Dispersal traits data extraction
##' @date 21/10/2015
##' @author Damien G.
##' @description This script will attempt to do some traits extraction from LECA 
##'   traits data base
##' @licence Script GPL but not the data produced!!
##' ----------------------------------------------------------------------------

rm(list = ls())
setwd("~/Work/BRISCA/workdir/")

library(dplyr)
library(tidyr)

## -- our species data ---------------------------------------------------------

## our sp.list
dat.sp <- read.table("../briscahub/data/sp.list_08102015_red.txt", 
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)

## extract also synonym list
##' @note cause some species has synonime names, we have to refer to a master file
##'   to be able to make the link btw try and our selected species names
master <- read.table("MASTER14.txt", 
                     sep="\t", na.strings=c("", "NA"), header = TRUE, dec = ".", 
                     fill= TRUE, stringsAsFactors = FALSE)

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

##' Traits linked to dispersal
leca.traits <- read.csv("leca_db_extraction.csv", stringsAsFactor = FALSE)
leca.traits$Libelle_short <- sapply(strsplit(leca.traits$Libelle, " "), 
                                    function(x){return(paste(x[1:2], collapse = " "))})
head(leca.traits)

leca.traits %>% filter(Code == "DISP") 
leca.traits %>% filter(Code == "DISP_VITTOZ") 
leca.traits %>% filter(Code == "DISP_INDK") 

##' Traits linked to maturity age
leca.traits.age <- read.csv("leca_db_extraction_age.csv", stringsAsFactor = FALSE)
leca.traits.age$Libelle_short <- sapply(strsplit(leca.traits.age$Libelle, " "), 
                                    function(x){return(paste(x[1:2], collapse = " "))})
head(leca.traits.age)

unique(leca.traits.age$Code)
leca.traits.age %>% group_by(Code) %>% summarise(n.species = n())
leca.traits.age %>% filter(Code == "AGEFLOW_MIN") 
leca.traits.age %>% filter(Code == "AGEFLOW_MAX") 
leca.traits.age %>% filter(Code == "MAX_AGE_INDK") 


## replace in this table synonyms by species names

leca.disp.traits <- leca.traits %>% 
  filter(is.element(Code, c("DISP", "DISP_VITTOZ", "DISP_INDK"))) %>%
  group_by(Libelle_short) %>%
  mutate(Genus.species = ifelse(is.element(unique(Libelle_short), master.sp$Synonym), 
                                  master.sp$Genus.species[master.sp$Synonym == unique(Libelle_short)],
                                  unique(Libelle_short)))

leca.disp.traits.spread <- leca.disp.traits %>% 
#   mutate(Value  = ifelse(!is.na(Valeur), as.character(Valeur), as.character(Nom))) %>%
  select(Libelle_short, Code, Nom) %>% 
  group_by(Libelle_short, Code) %>% 
  sample_n(1) %>% ## ensure that only one value by trait is selected
  as.data.frame %>%
  spread(Code, Nom) %>%
  mutate(Genus.species = Libelle_short) %>%
  select(-Libelle_short) %>%
  as.data.frame


## -- check Isa data base ------------------------------------------------------

isa.db <- read.table("SeedDispersal_Vittoz_SYNTHESE_jan2013.csv", header = TRUE, 
                     sep = ",", stringsAsFactors = FALSE, na.strings=c("", "NA"))
isa.db.sp.short <- sapply(strsplit(c(isa.db$Genus.species.subsp..LECA.CBNA., isa.db$NomCRSF..Vittoz.), " "), 
                          function(x){return(paste(x[1:2], collapse = " "))})
isa.db.sp.short <- isa.db.sp.short %>% unique

isa.db.sp.short.sp <- intersect(isa.db.sp.short, sp.list.full.short)

unique(cbna.valid.sp$Libelle_short)
unique(isa.db.sp.short.sp)

intersect(cbna.valid.sp$Libelle_short, isa.db.sp.short.sp)
setdiff(cbna.valid.sp$Libelle_short, isa.db.sp.short.sp)

### NOT keep because all species are repeted within leca database

## -- Manually kew extracted dataset -------------------------------------------
kew.db <- read.table("disp_traits_from_kew.csv", header = TRUE, 
                     sep = "\t", stringsAsFactors = FALSE, na.strings=c("", "NA"))

## -- Manually flora indicativa extracted dataset ------------------------------
indkman.db <-read.table("disp_traits_from_flora_indic.csv", header = TRUE, 
                        sep = "\t", stringsAsFactors = FALSE, na.strings=c("", "NA"))

## -- try extracted dataset ----------------------------------------------------
try.db <-read.table("disp_traits_from_try.csv", header = TRUE, 
                        sep = "\t", stringsAsFactors = FALSE, na.strings=c("", "NA"))

## -- build a meta table containing all the dispersal info collected so far ---

disp.tab <- sp.list %>% 
  left_join(leca.disp.traits.spread) %>%
  left_join(kew.db) %>%
  left_join(indkman.db) %>%
  left_join(try.db) %>%
  as.data.frame

write.table(disp.tab, "sp_list_red_whith_disp_traits.csv", col.names = TRUE, row.names = FALSE, sep = "\t")

head(disp.tab)

## -- some summary of this table -----------------------------------------------
rm(list = ls())
setwd("~/Work/BRISCA/workdir/")

library(dplyr)
library(tidyr)

disp.tab <- read.table("sp_list_red_whith_disp_traits.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

disp.tab.wg <- disp.tab %>% 
  mutate(Genus = sub("[[:space:]].*$", "", Genus.species),
         AsOneDispTrait = !(is.na(DISP) & is.na(DISP_INDK) & is.na(DISP_VITTOZ) & 
                              is.na(DISP_KEW) & is.na(DISP_INDKMAN) & is.na(DISP_TRY)) ) 

## nb.species having at least 1 disp trait 
sum(disp.tab.wg$AsOneDispTrait) ## 64 / 189
  
## nb of genus that have at least one disp trait
length(unique(disp.tab.wg$Genus)) ## 49 Genus in the dataset
disp.tab.wg %>%
  group_by(Genus) %>% 
  summarise(AsOneDispTraitGenus = any(AsOneDispTrait)) %>% 
  ungroup %>%
  summarize(NbGenusWithATrait = sum(AsOneDispTraitGenus)) ## 34/49

## nb of species that have at least one trait by genus
disp.tab.wg %>%
  group_by(Genus) %>% 
  summarise(nb.trait.for.genus = sum(AsOneDispTrait)) %>% 
  as.data.frame


## -- try to model max dispersal distance --------------------------------------

##' @note we will use the dispeRsal function from Tamme et al. 2014

## load the full info 
disp.tab.tamme.full <- read.csv("sp_list_red_whith_disp_traits_for_dispeRsal_bis.csv", 
                                stringsAsFactors = FALSE, sep  = "\t")
disp.tab.tamme.full$SM.try[disp.tab.tamme.full$SM.try == "#VALEUR !"] <- NA
disp.tab.tamme.full$SM.kew[disp.tab.tamme.full$SM.kew == "#VALEUR !"] <- NA

## reshape the table to keep only needed info
disp.tab.tamme <- disp.tab.tamme.full %>% 
  select(Species, DS, GF, RH, SM.kew, SM.try, TV) %>%
  mutate(SM = ifelse(!is.na(SM.try), SM.try, SM.kew)) %>%
  select(-c(SM.try, SM.kew))

disp.tab.tamme$Species <- as.factor(disp.tab.tamme$Species)
disp.tab.tamme$DS <- as.factor(disp.tab.tamme$DS)
disp.tab.tamme$GF <- as.factor(disp.tab.tamme$GF)
disp.tab.tamme$RH <- as.numeric(disp.tab.tamme$RH)
disp.tab.tamme$SM <- as.numeric(disp.tab.tamme$SM)
disp.tab.tamme$TV <- as.numeric(disp.tab.tamme$TV)

## fill missing data by extrapolating ------------------------------------------

## extrapolating rules ---
## 1. genus / growth form
## 2. familly / growth form
## 3. growth form

## check the gap we have to fill
sum(!is.na(disp.tab.tamme$DS)) ## 63 / 189
sum(!is.na(disp.tab.tamme$GF)) ## 189 / 189
sum(!is.na(disp.tab.tamme$RH)) ## 189 / 189
sum(!is.na(disp.tab.tamme$SM)) ## 172 / 189

## load the function
load("dispeRsal.rda")

## define the family level of our species
sp.family.tab <- TPLMod(splist = as.character(disp.tab.tamme$Species))

## merge data 
disp.tab.tamme$Genus.species <- disp.tab.tamme$Species
disp.tab.tamme$Genus <- sub(" .*", "", disp.tab.tamme$Genus.species)
disp.tab.tamme$Species <- sub("^.* ", "", disp.tab.tamme$Genus.species)
disp.tab.tamme <- disp.tab.tamme %>% full_join(sp.family.tab)

## check dispersal syndrom representation in our table
disp.tab.tamme %>% group_by(Genus.species, GF) %>% 
  summarise(has.DS = any(!is.na(DS)),
            has.SM = any(!is.na(SM))) %>%
  ungroup %>%
  summarise(pc.spe.with.DS = mean(has.DS),
            pc.spe.with.SM = mean(has.SM))

disp.tab.tamme %>% group_by(Genus, GF) %>% 
  summarise(has.DS = any(!is.na(DS)),
            has.SM = any(!is.na(SM))) %>%
  ungroup %>%
  summarise(pc.gen.with.DS = mean(has.DS),
            pc.spe.with.SM = mean(has.SM))

disp.tab.tamme %>% group_by(Family, GF) %>% 
  summarise(has.DS = any(!is.na(DS)),
            has.SM = any(!is.na(SM))) %>%
  ungroup %>%
  summarise(pc.fam.with.DS = mean(has.DS),
            pc.spe.with.SM = mean(has.SM))

## fill the seed mass and  info
disp.tab.tamme$SM.extrapol <- 0 ## no extrapolation

Traits.extrap <- disp.tab.tamme %>% 
  mutate(SM.Species = SM,
         DS.Species = as.character(DS)) %>%
  group_by(Genus, GF) %>% 
  mutate(SM.mean.GenusGF = mean(SM, na.rm = TRUE),
         SM.median.GenusGF = median(SM, na.rm = TRUE),
         DS.maj.GenusGF = names(table(DS))[which.max(table(DS))]) %>%
  ungroup %>% group_by(Family, GF) %>%
  mutate(SM.mean.FamilyGF = mean(SM, na.rm = TRUE),
         SM.median.FamilyGF = median(SM, na.rm = TRUE),
         DS.maj.FamilyGF = names(table(DS))[which.max(table(DS))]) %>%
  ungroup %>% group_by(GF) %>%
  mutate(SM.mean.GF = mean(SM, na.rm = TRUE),
         SM.median.GF = median(SM, na.rm = TRUE),
         DS.maj.GF = names(table(na.omit(DS)))[which.max(table(na.omit(DS)))]) %>%
  ungroup %>% rowwise %>%
  mutate(SM.extrap = ifelse(!is.na(SM.Species), SM.Species, 
                            ifelse(!is.na(SM.median.GenusGF), SM.median.GenusGF,
                                   ifelse(!is.na(SM.median.FamilyGF), SM.median.FamilyGF,
                                          SM.median.GF))),
         SM.extrap.level = ifelse(!is.na(SM.Species), "Species", 
                            ifelse(!is.na(SM.median.GenusGF), "Genus",
                                   ifelse(!is.na(SM.median.FamilyGF), "Family",
                                          "GrowthForm"))),
         DS.extrap = ifelse(!is.na(DS.Species), DS.Species, 
                            ifelse(!is.na(DS.maj.GenusGF), DS.maj.GenusGF,
                                   ifelse(!is.na(DS.maj.FamilyGF), DS.maj.FamilyGF,
                                          DS.maj.GF))),
         DS.extrap.level = ifelse(!is.na(DS.Species), "Species", 
                            ifelse(!is.na(DS.maj.GenusGF), "Genus",
                                   ifelse(!is.na(DS.maj.FamilyGF), "Family",
                                          "GrowthForm")))
         ) %>%
  as.data.frame

head(Traits.extrap)

## extract a subset of the extrapolated version of traits to estimate disersal 
## distances

disp.tab.tamme.extrap <- Traits.extrap %>% 
  mutate(Species = Genus.species,
         DS = as.factor(DS.extrap),
         GF = GF,
         SM = SM.extrap,
         RH = RH) %>%
  select(Species, DS, GF, SM, RH)

## --- compute the model to estimate maximal dispersal distance ----------------
disp.mod <- dispeRsal(data.predict = disp.tab.tamme.extrap, 
                      model = 2, 
                      CI = TRUE,
                      random = TRUE, 
                      tax = "family", 
                      write.result = TRUE)

## reorder the table to fit with our ref table, add a key to make the jointing easier
## and save it!
dat.sp <- read.table("../briscahub/data/sp.list_08102015_red.txt", 
                     header = TRUE, sep = "\t", stringsAsFactors = FALSE)

dat.disp <- disp.mod$predictions
##' @note it seems that Salix ovalifolia disapear from this table but because this
##'   species has exactly the same input params than Salix rotundifolia, we will
##'   just make a copy of this species dispersal limitation predictions
sal.ova.disp <- dat.disp %>% filter(Species == "Salix rotundifolia") %>% mutate(Species = "Salix ovalifolia")
dat.disp <- dat.disp %>% bind_rows(sal.ova.disp)

## rename Species column to simplify jointing
dat.disp <- dat.disp %>% rename(Genus.species = Species)

dat.disp <- dat.disp %>% left_join(select(dat.sp, Genus.species, Biomod.name))

## check that all matches have been done
dat.disp <- dat.disp %>% arrange(Biomod.name)
sum(is.na(dat.disp$Biomod.name))
dim(dat.disp)

## write this table on hard drive
write.table(dat.disp, file = "../briscahub/data/sp.list_red_tamme_disp.txt", col.names = TRUE, row.names = FALSE, quote = TRUE, sep = "\t")

## --- do some plots to illustarte the results ---------------------------------
library(ggplot2)

## MDD ~ species
gg <- ggplot(data = disp.mod$predictions, aes(x = Species, y = log10MDD, ymin = log10MDD_lwrCL, ymax =  log10MDD_uppCL, col = Family)) + 
  geom_errorbar() + geom_point() + coord_flip()

png("../figures/species_max_dispersal_distance_tamme.png", width = 800, height = 2000, units = 'px')
print(gg)
dev.off()


dat.mr.mdd <- disp.mod$predictions %>%
  mutate(Genus.species = as.character(Species)) %>%
  inner_join(disp.tab) %>%
  filter(!is.na(Mig.rate.m.yr))
## MDD ~ migr rate
gg <- ggplot(data = dat.mr.mdd, aes(x = log10(Mig.rate.m.yr), y = log10MDD, ymin = log10MDD_lwrCL, ymax =  log10MDD_uppCL, col = Family)) + 
  geom_errorbar(alpha = .3) + geom_point() + 
#   geom_smooth(method = 'lm', col = 'black') +
  geom_abline(intercept = 0, slope = 1, linetype = 3)

gg

## migr rate ~ species
gg <- ggplot(data = dat.mr.mdd, aes(x = Species, y = log10(Mig.rate.m.yr), col = Family)) + 
  geom_point() + coord_flip()
gg
