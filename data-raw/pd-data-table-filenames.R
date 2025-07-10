# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R

create_data_hake("data_tables_path", "data-tables")

# Maturity and weight-at-age ----
create_data_hake("maturity_samples_fn", "maturity-samples.csv")
create_data_hake("maturity_estimates_fn", "maturity-ogives.csv")
create_data_hake("weight_age_sample_sizes_fn","wtatage-all-samplesize.csv")
create_data_hake("weight_age_estimates_fn", "weight-at-age-ogives.csv")

# Age proportions ----
create_data_hake("age_props_fn", "age-proportions.csv")
create_data_hake("raw_age_counts_fn", "raw-age-counts.csv")

# Catch data ----
create_data_hake("catch_by_month_fn", "catch-by-month.csv")
create_data_hake("catch_by_year_fn", "catch-by-year.csv")
create_data_hake("landings_tac_fn", "landings-tac-history.csv")
create_data_hake("further_tac_details_fn", "further-tac-details.csv")
create_data_hake("catch_targets_biomass_fn", "catch-targets-biomass.csv")

# Weight-at-age
create_data_hake("waa_fn", "weight-at-age.csv")
create_data_hake("survey_waa_fn", "survey-weight-at-age.csv")

# Survey data ----
create_data_hake("kriging_pars_fn", "kriging-parameters.csv")
create_data_hake("survey_history_fn", "survey-history.csv")
create_data_hake("survey_by_country_fn", "survey-by-country.csv")

# Depth data ----
create_data_hake("bottom_depth_fn", "depth-bottom.csv")
create_data_hake("gear_depth_fn", "depth-gear.csv")

# Overview map data ----
create_data_hake("ports_fn", "map-data/port-locations.csv")
create_data_hake("states_fn", "map-data/state-locations.csv")

# Sample and landing data ----
create_data_hake("sample_dr", "/srv/hake-sog/other/samples")
create_data_hake("landings_dr", "/srv/hake-sog/other/landings")

create_data_hake("sample_data_rds_fn", "sample-data.rds")
create_data_hake("depths_rds_fn", "depths-fishing-events.rds")

