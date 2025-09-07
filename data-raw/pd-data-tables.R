# Source this file to apply changes. Need load_all(). Don't forget to add documentation for
# any new package data to file R/data.R

load_dir <- here("data-tables")
# Maturity and weight-at-age ----
create_data_hake("maturity_samples_df",
                 read_csv(file.path(load_dir,
                                    "maturity-samples.csv"),
                          guess_max = Inf,
                          show_col_types = FALSE))

create_data_hake("maturity_estimates_df",
                 read_csv(file.path(load_dir,
                                    "maturity-ogives.csv"),
                          guess_max = Inf,
                          show_col_types = FALSE))

create_data_hake("weight_at_age_estimates_df",
                 read_csv(file.path(load_dir,
                                    "weight-at-age-ogives.csv"),
                          guess_max = Inf,
                          show_col_types = FALSE))
create_data_hake("weight_age_sample_sizes_df",
                 read_csv(file.path(load_dir,
                                    "wtatage-all-samplesize.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# Catch and TAC ----
create_data_hake("landings_tac_df",
                 read_csv(file.path(load_dir,
                                    "landings-tac-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE) |>
                   rename(`Catch (t)` = Catch,
                          `TAC (t)` = TAC) |>
                   mutate(`Attainment (\\%)` = `Catch (t)` / `TAC (t)` * 100))

create_data_hake("catch_targets_df",
                 read_csv(file.path(load_dir,
                                    "catch-targets-biomass.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("further_tac_df",
                 read_csv(file.path(load_dir,
                                    "further-tac-details.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

create_data_hake("catch_by_month_df",
                 read_csv(file.path(load_dir,
                                    "catch-by-month.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# Sampling history data ----
create_data_hake("sampling_history_df",
                 read_csv(file.path(load_dir,
                                    "fishery-sampling-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# * Canada age proportions ----
create_data_hake("age_df",
                 read_csv(file.path(load_dir,
                                    age_props_fn),
                          col_types = cols(),
                          show_col_types = FALSE))

# Survey data ----
create_data_hake("kriging_pars_df",
                 read_csv(file.path(load_dir, "kriging-parameters.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

create_data_hake("survey_indices_df",
                 read_csv(file.path(load_dir,
                                    "survey-indices.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE) |>
                   rename(`Index (t)` = Index))

# Depth data ----
# * Canada depths ----
create_data_hake("bottom_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-bottom.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("gear_depth_df",
                 read_csv(file.path(load_dir,
                                    "depth-gear.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

# Overview map data ----
create_data_hake("ports_df",
  read_csv(file.path(load_dir,
                     "map-data",
                     "port-locations.csv"),
           col_types = cols(),
           comment = "#",

           show_col_types = FALSE))

create_data_hake("states_df",
                 read_csv(file.path(load_dir,
                                    "map-data",
                                    "state-locations.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

# Weight-at-age data
create_data_hake("weight_at_age_df",
                 read_csv(file.path(load_dir, waa_fn),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

