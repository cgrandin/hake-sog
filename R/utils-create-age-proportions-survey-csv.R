#' Create an age proportion CSV data from the sample data
#'
#' @details
#' A CSV file will be written, and the data frame will also be returned
#' invisibly. The data frame will contain the year, number of fish by year
#' in the data, and the number of samples (hauls/trips) in the data.
#' Shoreside uses trips as a sampling unit and the other two use hauls.
#'
#' @param d A data frame as returned by [extract_survey_samples_sog()]
#' @param min_date Earliest date to include
#' @param raw_counts Logical. If `TRUE`, return raw, unweighted age
#' proportions. If `FALSE`, return the age proportions weighted by sample
#' and catch weights
#' @param plus_grp Age plus group for maximum grouping
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#' @param weight_scale A value to divide the weights by
#' @param by_month Logical. If `TRUE`, return a data frame with a
#' `month` column in addition to a `year` column
#' @param digits The number of decimal places to round the values to
#'
#' @return The age proportion data frame, invisibly
#' @export
create_age_proportions_survey_csv <- function(
    d,
    min_date = as.Date("1979-01-01"),
    raw_counts = FALSE,
    plus_grp = 15,
    lw_tol = 0.1,
    lw_maxiter = 1e3,
    weight_scale = 1e3,
    by_month = FALSE,
    digits = 5){


  temporal_grouping <- if(by_month) c("year", "month") else "year"

  if(!"trip_start_date" %in% names(d)){
    d <- d |>
      rename(trip_start_date = trip_end_date)
  }

  d <- d |>
    dplyr::filter(!is.na(age)) |>
    mutate(age = ifelse(age > plus_grp, plus_grp, age)) |>
    mutate(trip_start_date = as.Date(trip_start_date)) |>
    dplyr::filter(trip_start_date >= min_date)

  if(by_month){
    d <- d |>
      mutate(month = month(trip_start_date)) |>
      select(year, month, sample_id, length,
             weight, age)
  }else{
    d <- d |>
      select(year, sample_id, length, weight,
             age)
  }

  if(raw_counts){

    out <- d |>
      dplyr::filter(!is.na(age)) |>
      dplyr::filter(age > 0) |>
      group_by(year) |>
      mutate(num_at_age_year = n()) |>
      ungroup() |>
      group_by(year, age) |>
      mutate(num_at_age = n()) |>
      ungroup()|>
      select(year, age, num_at_age, num_at_age_year) |>
      distinct() |>
      select(year, age, num_at_age) |>
      arrange(year, age) |>
      complete(year, age = 1:plus_grp, fill = list(age_prop = 0)) |>
      pivot_wider(names_from = age, values_from = num_at_age) |>
      mutate(across(-year, ~{ifelse(is.na(.x), 0, .x)})) |>
      mutate(across(-year, ~{round(.x, digits)}))

    fn <- here(data_tables_path, raw_age_counts_fn)

    write_csv(out, fn)
    message("The file:\n`", fn, "`\nwas written with new age proportion ",
            "data\n")
    return(invisible(out))
  }

  all_yrs_lw <- fit_lw(d, lw_tol, lw_maxiter)

  ds <- d |>
    calc_lw_params("sample_id", lw_tol, lw_maxiter) |>
    calc_lw_params(temporal_grouping, lw_tol, lw_maxiter) |>
    rename(lw_alpha.x = lw_alpha,
           lw_beta.x = lw_beta) |>
    mutate(lw_alpha.y = all_yrs_lw[1],
           lw_beta.y = all_yrs_lw[2]) |>
    mutate(lw_alpha = coalesce(lw_alpha.x, lw_alpha.y),
           lw_beta = coalesce(lw_beta.x, lw_beta.y)) |>
    select(-c(lw_alpha.x, lw_alpha.y, lw_beta.x, lw_beta.y))

  # Calculate the weights from length for all missing weights,
  # using specimen-specific LW params
  ds <- ds |>
    dplyr::filter(!is.na(length)) |>
    mutate(weight = ifelse(is.na(weight),
                           lw_alpha * length ^ lw_beta,
                           weight))

  num_ages_by_yr_df <- ds |>
    group_by(year) |>
    summarize(num_fish = n()) |>
  ungroup()

  num_samples_by_yr_df <- ds |>
    distinct(year, sample_id) |>
    group_by(year) |>
    summarize(num_samples = n()) |>
    ungroup()

  age_by_year_df <- ds |>
    dplyr::filter(age > 0) |>
    select(year, age) |>
    group_by(year, age) |>
    summarize(num_at_age = n()) |>
    ungroup() |>
    complete(year,
             age = 1:plus_grp, fill = list(num_at_age = 0)) |>
    left_join(num_ages_by_yr_df, by = "year") |>
    mutate(age_prop = num_at_age / num_fish)

  age_props <- age_by_year_df |>
    select(-c(num_at_age, num_fish)) |>
    pivot_wider(names_from = age, values_from = age_prop) |>
    mutate(across(-year, ~{f(.x, digits)})) |>
    left_join(num_ages_by_yr_df, by = "year") |>
    left_join(num_samples_by_yr_df, by = "year") |>
    select(year, num_fish, num_samples, everything())

  # Write the csv files out ----
  fn <- here(data_tables_path, age_props_survey_fn)

  write_csv(age_props, fn)
  message("The file:\n`", fn, "`\nwas written with new age proportion data\n")

  invisible(age_props)
}
