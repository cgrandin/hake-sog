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
create_data_hake("ct",
                 read_csv(file.path(load_dir,
                                    "landings-tac-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE) |>
                   mutate(`U.S. Total` =
                            `U.S. Foreign` +
                            `U.S. Joint-venture` +
                            `U.S. Mothership` +
                            `U.S. Catcher-processor` +
                            `U.S. Shoreside` +
                            `U.S. Research`,
                          `Canada Total` =
                            `Canada Foreign` +
                            `Canada Joint-venture` +
                            `Canada Shoreside` +
                            `Canada Freezer-trawler`,
                          Total = `U.S. Total` + `Canada Total`,
                          us_prop = `U.S. Total` / Total * 100,
                          can_prop = `Canada Total` / Total * 100,
                          us_attain = `U.S. Total` / `U.S. TAC` * 100,
                          can_attain = `Canada Total` / `Canada TAC` * 100,
                          tot_attain = `Total` / `Total TAC` * 100))

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

create_data_hake("survey_history_df",
                 read_csv(file.path(load_dir,
                                    "survey-history.csv"),
                          col_types = cols(),
                          show_col_types = FALSE))

create_data_hake("survey_by_country_df",
                 read_csv(file.path(load_dir,
                                    "survey-by-country.csv"),
                          col_types = cols(),
                          comment = "#",
                          show_col_types = FALSE))

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
weight_at_age_df <- dplyr::bind_rows(
  utils::read.csv(
    fs::path(load_dir, can_waa_fn)
  ) |>
    dplyr::mutate(data_type = "fishery", country = "canada"),
  utils::read.csv(
    fs::path(load_dir, "us-weight-at-age.csv")
  ) |>
    dplyr::mutate(
      data_type = "fishery",
      country = "usa",
      # Set the month of acoustic poland survey data to 8 based on literature
      Month = ifelse(Source == "Acoustic Poland", 8, Month)
    ),
  utils::read.csv(
    fs::path(load_dir, "survey-weight-at-age.csv")
  ) |>
    dplyr::mutate(
      data_type = "survey",
      country = ifelse(Source == "Canada Acoustic", "canada", "usa")
    ) |>
    dplyr::filter(
      Weight_kg > 0.001
    )
) |>
  dplyr::select(-dplyr::matches("length")) |>
  dplyr::mutate(
    Sex = tidyr::replace_na(Sex, "U")
  ) |>
  dplyr::rename(weight = Weight_kg, age = Age_yrs) |>
  dplyr::rename_with(.fn = tolower)
create_data_hake("weight_at_age_df", weight_at_age_df)
