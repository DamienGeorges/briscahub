setwd('/work/georges/BRISCA/')
library(raster)

## get the job parameter
args <- commandArgs(trailingOnly = TRUE)
job_id_ <- as.numeric(args[1])

out_dir <- '/work/georges/BRISCA/workdir/checkRasPix'
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## load the parameter table 
in_dir <- '/work/georges/BRISCA/workdir/check_nb_pix_params'
d_files <- read.csv(file.path(in_dir, paste0("d_sub_", job_id_, '.csv')))

## the main function
nb.pix <- function(x) sum(!is.na(raster(x)[]))

## get the list of raster we want to investigate
test_pix <- sapply(d_files$file, nb.pix)
test_pix_df <- data.frame(file = names(test_pix), nb_pix = test_pix, job_id = job_id_)
write.csv(test_pix_df, file = file.path(out_dir, paste0('test_pix_df_', job_id_, '.csv')))


cat('\n\ncompleted!')

q('no')


