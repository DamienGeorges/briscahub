library(dplyr)
xx <- read.table("data/sp.list_03.03.2017.txt", sep = "\t", h =T, stringsAsFactors = F)

# Tall shrub (~50 cm or greater)
# Low shrub (~20–50 cm)
# Dwarf shrub (less than ~20 cm)
xx$Growth.form.Isla <- xx$Growth.form.height
xx$Growth.form.Isla[xx$Growth.form.height == "TREE"] <- "Tree"
xx$Growth.form.Isla[xx$Growth.form.height == "SHRUB" & xx$All.height.median >= 0.5] <- "Tall shrub"
xx$Growth.form.Isla[xx$Growth.form.height == "SHRUB" & xx$All.height.median < 0.5 & xx$All.height.median >= 0.2] <- "Low shrub"
xx$Growth.form.Isla[xx$Growth.form.height == "SHRUB" & xx$All.height.median < 0.2] <- "Dwarf shrub"

xx %>% select(Growth.form.height, All.height.median, Growth.form.Isla)

write.table(xx, file = "data/sp.list_03.03.2017.txt", sep = "\t", col.names = TRUE, row.names = FALSE)
