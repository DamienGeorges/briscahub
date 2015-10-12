##' -- header ------------------------------------------------------------------
##' @title Clean biomod pure climate directory
##' @date 12/10/2015
##' @author damien g.
##' @description Cause we had to model the link btw our species distribution and
##'   the climate in several steps (species list change, error in thining process, ...)
##'   We have to clean the modeling directory to keep only the final and good version
##'   of our models.
##' -- end of header -----------------------------------------------------------

## -- start of script ----------------------------------------------------------

rm(list = ls())
setwd("~/BRISCA/workdir")

## get the final list of species we want to model
sp.tab <- read.table("../briscahub/data/sp.list_08102015.txt", sep = "\t", 
                     header = TRUE, stringsAsFactors = FALSE)
head(sp.tab)

## we want to keep only species having at least 20 occurences
sp.tab.red <- sp.tab[!(is.na(sp.tab$total.occ) | sp.tab$total.occ < 20), ]

## update Biomoo.name column
sp.tab.red$Biomod.name <- gsub("-", "", sp.tab.red$Genus.species)
sp.tab.red$Biomod.name <- gsub(" ", ".", sp.tab.red$Biomod.name, fixed = "TRUE")
sp.tab.red$Biomod.name <- gsub("_", ".", sp.tab.red$Biomod.name, fixed = "TRUE")

## get the latest version of biomod pure climate models and put them in a new 
## directory. The priority is to search into "/work/georges/BRISCA/Biomod_pure_climate_usgs/"
## then into "/work/georges/BRISCA/Biomod_pure_climate/" 

out.dir <- "/work/georges/BRISCA/Biomod_pure_climate_final"
dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)

in.dir.1 <- "/work/georges/BRISCA/Biomod_pure_climate_usgs/"
in.dir.2 <- "/work/georges/BRISCA/Biomod_pure_climate/"

sp.in.dir <- sapply(sp.tab.red$Biomod.name, function(x){
  out <- character(0)
  ## is the species models in the prioritary dir ?
  out <- grep(paste0("/", x, "$"), list.files(in.dir.1, full.names = TRUE), value = TRUE)
  ## is the species models in the secondary dir ?
  if(!length(out)){
    out <- grep(paste0("/", x, "$"), list.files(in.dir.2, full.names = TRUE), value = TRUE)  
  }
  return(out)
  })

## check if some species are missing
names(sp.in.dir)[sapply(sp.in.dir, function(x) !length(x))]

## move all wanted modeilling directory in the final directory
lapply(sp.in.dir, function(x){
  cat('\n> moving', x, '...')
  file.rename(x, file.path(out.dir, basename(x)))
})

sp.copied <- list.files(out.dir)
setdiff(sp.tab.red$Biomod.name, sp.copied)

sp.tab.red$Biomod.name[duplicated(sp.tab.red$Biomod.name)]
sp.tab[is.element(sp.tab$Biomod.name, sp.tab$Biomod.name[duplicated(sp.tab$Biomod.name)]), ]


## -- end of script ------------------------------------------------------------
