#!/bin/bash -x

set -e
source /applis/ciment/v2/env.bash
export PATH=$PATH:/usr/local/bin

## TODO : FILL NAME FIELD AUTOMATICALLY ##
NAME=scoidra

# List of need modules =-=-=-=-=-=-=-=-=-=-=-=-=-=- #
# module load zlib
# module load gzip
# module load netcdf
module load proj
module load gdal
module load R
# End list of need modules =-=-=-=-=-=-=-=-=-=-=-=- #

# Do some checks =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
if [ "$1" = "" ]
then
  echo "Missing parameter"
  exit 1
fi

PWD=`pwd`
NATIVE_DIR="$PWD"

# Create outputs directories architecture =-=-=-=- #
######## Run in scratch #########
## Create scratch working directory 
#mkdir -p $SCRATCH_DIR/$LOGNAME/$NAME
#cd $SCRATCH_DIR/$LOGNAME/$NAME
#chmod -R +x $SCRATCH_DIR/$LOGNAME/$NAME
###### End run in scratch #######



####################################################
######## Main job to execute #######################


R CMD BATCH  "--args ${1}" /home/dgeorges/BRISCA/briscahub/scripts/0_data_formatting/create_species_occ_inv_distances_rasters_alternative.R  /nfs_scratch2/dgeorges/BRISCA/LOGFILES/${NAME}_${OAR_JOB_NAME}_${1}_${OAR_JOBID}.Rout

######## End main job to execute ###################
####################################################




