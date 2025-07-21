#' Create a CSV file with catch by year and month
#'
#' @param df A data.frame as returned by [extract_catch()]
#' @param catch_scale A value to divide the catch by prior to writing to
#' the file
#' @param digits The number of decimal points to report on the catches in
#' the csv files
#'
#' @returns Nothing, creates three CSV files
#' @export
create_month_catch_csv_file <- function(df,
                                        catch_scale = 1000,
                                        digits = 3){

  if(is.list(df)){
    df <- df[[1]]
  }

  fn <- here(data_tables_path, catch_by_month_fn)

  d <- df |>
    group_by(year, month) |>
    summarize(catch = sum(landings) / catch_scale) |>
    ungroup() |>
    mutate(catch = round(catch, digits)) |>
    pivot_wider(names_from = "month", values_from = "catch") |>
    select(year, as.character(1:12)) |>
    pivot_longer(-year, names_to = "month", values_to = "catch") |>
    select(month, year, catch) |>
    mutate(catch = ifelse(is.na(catch), 0, catch))


  write_csv(d, fn)
  message("The file:\n`", fn, "`\nwas written with new catch data\n")
}
