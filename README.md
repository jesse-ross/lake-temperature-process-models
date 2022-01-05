# GLM modeling of lake temperatures
This repository is for running uncalibrated GLM models of lake temperatures.

-----------------
## Dependent files from [`lake-temperature-model-prep pipeline`](https://github.com/USGS-R/lake-temperature-model-prep)
_Files that will eventually be transferred using GLOBUS:_
  * Lake - GCM cell crosswalk: `'1_prep/in/lake_cell_xwalk.csv'`
    * Created within [`gcm_driver_data_munge_pipeline` branch](https://github.com/USGS-R/lake-temperature-model-prep/tree/gcm_driver_data_munge_pipeline) of repo
  * List of lake-specific attributes for nml modification: `'1_prep/in/nml_list.rds'`
  * Munged GCM netCDF files (not yet brought in manually, see below)

_Files used in current testing development phase:_
  * In place of munged GCM netCDF files, manually bringing in feather files created by Lindsay for each GCM type, GCM cell, and time period: `'1_prep/tmp/GCM_{gcm name}_{gcm time period}_{gcm cell number}.feather'`
  
-----------------

## Running the pipeline on HPC, in parallel

### Denali quickstart
```R
ssh denali.cr.usgs.gov
cd /caldera/projects/usgs/water/iidd/datasci/lake-temp/lake-temperature-process-models

# to change user permissions for collaboration
umask 002
```

### Shifter
For any of the following applications, you'll need the shifter module:

```R
module load shifter
```
Here's how to get the image that Jesse built from Dockerhub and translate it to shifter (this should only need to be done once per image):
```R
shifterimg pull docker:jrossusgs/glm3r:v0.6d
```
Here's how to build targets using the shifter container and the `targets::tar_make_clustermq(target_name, workers=n_workers)` option to build targets in parallel within the shifter container, with a specified number of workers (up to 80, as Denali has 80 cores per node). Targets will then delegate work out to `n_workers` cores for any parallelizable step that you don't specifically tell it to run in serial.
```R
salloc --cpus-per-task=79 --image=docker:jrossusgs/glm3r:v0.6d -t 00:30:00 -A watertemp shifter Rscript -e 'targets::tar_make_clustermq(p2_glm_uncalibrated_runs, workers=79)'
```
Here's how to run the shifter container interactively on an allocated job:
```R
salloc -c 1 --image=docker:jrossusgs/glm3r:v0.6d -t 00:30:00 -A watertemp shifter /bin/bash
# when you're in the shifter environment, the prompt starts with "I have no name!@"
R
library(targets)
tar_make_clustermq(p2_glm_uncalibrated_runs, workers=79)
# etc
q()
exit
```
-----------------

## Running the pipeline locally, in serial
You can simply build targets as normal, using `tar_make()`, and `targets` will ignore the `cluster_mq.scheduler` options set in `'_targets.R'`

## Running the pipeline locally, in parallel
The pipeline can be run in parallel locally through docker, just as it can be run through shifter on denali.

Simple command-line R interface:
```bash
cd ~/lake-temperature-process-models
docker run -v '/home/jross/lake-temperature-process-models/:/lakes' -it jrossusgs/glm3r:v0.6d R
## Now you have an R prompt in the container, with the project directory mounted at `/lakes/`.
```

Or alternatively, you could run RStudio in the container and access it through your browser (user is rstudio, password set in the startup command as mypass).
```bash
cd ~/lake-temperature-process-models
docker run -v '/home/jross/lake-temperature-process-models/:/lakes' -p 8787:8787 -e PASSWORD=mypass -e ROOT=TRUE -d jrossusgs/glm3r:v0.6d
```

```r
setwd("/lakes") 
# Do a lot of work at once and test your computer's fan
targets::tar_make_clustermq(p2_glm_uncalibrated_runs, workers = 32)
```