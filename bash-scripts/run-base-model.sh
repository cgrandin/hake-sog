#!/bin/bash

# Run base model MCMC using ADNUTS
# To fix the permissions for the hake group:
# chmod -R u+rwx,g+rwx,o= 01-base-models

# Start timer
SECONDS=0

# Create the variable $assess_year containing the current year unless it
# is currently December, in which case it will be the current year + 1
# Enter a year as an argument here to force it to be that year, even if
# December.
. ./get-assess-year.sh

# The path structure is as follows
# /srv/hake/models/2023/01-version/01-base-models/01-base/
#  ^   ^    ^      ^    ^          ^              ^
#  |   |    |      |    |          |              |
#  |   |    |      |    |          |              $model_name
#  |   |    |      |    |          $type_path
#  |   |    |      |    $version_path
#  |   |    |      $year
#  |   |    $models_path
#  \  /
#   ||
#   $project_path

repo_path="/home/grandin/github/pacific-hake/hake-sog"
# If running on a local machine and the model folder is in your
# repo root, uncomment the next line and comment the line after it
#models_path=$repo_path/models
models_path="/srv/hake-sog/models"
# *Never* change `year_path` manually - See `get-assess-year.sh` call above
year_path=$assess_year
version_path="01-version"
type_path="01-base-models"
model_name="01-base"

ss_exe="ss3"
num_chains=8
num_samples=2000
num_warmup_samples=250
adapt_delta=0.8
run_extra_mcmc=TRUE

model_path=$models_path/$year_path/$version_path/$type_path/$model_name

[[ ! -d $model_path ]] && { echo "Error: Directory $model_path does not \
exist, bailing out." ; exit 1; }

(
  trap 'kill 0' SIGINT; \
    Rscript -e " \
    setwd('$repo_path'); \
    devtools::load_all(); \
    devtools::load_all('../hake-assessment');
    run_adnuts_timed('$model_path', \
                     adapt_delta = $adapt_delta, \
                     run_extra_mcmc = $run_extra_mcmc, \
                     num_chains = $num_chains, \
                     num_samples = $num_samples, \
                     num_warmup_samples = $num_warmup_samples, \
                     fn_exe = '$ss_exe')" \
  echo "Base model MCMC complete" \
)

ELAPSED="Script runtime: $(($SECONDS / 3600)) hrs $((($SECONDS / 60) % 60)) \
min $(($SECONDS % 60)) sec"

echo $ELAPSED
