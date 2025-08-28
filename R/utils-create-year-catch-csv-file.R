#' Calculate the catch by year
#'
#' @param df A data.frame as returned by [extract_catch_sog()]
#' @param catch_scale A value to divide the catch by prior to writing to
#' the file
#' @param write_file If `TRUE`, write the output to the data file given by
#' the package data variable `catch_by_year_fn`. If `FALSE`, return the
#' data frame
#' @param digits The number of decimal places to include in the table output
#'
#' @return If `write_file` is `TRUE`, nothing. If `write_file` is `FALSE`,
#' a data frame containing the catch by year (rows) and fishery type (columns)
#' @export
create_year_catch_csv_file <- function(df,
                                       catch_scale = 1000,
                                       write_file = TRUE,
                                       digits = 2){

  if(is.list(df)){
    df <- df[[1]]
  }

  catch_by_yr <- df |>
      group_by(year) |>
      summarize(total_weight = sum(landings)) |>
      ungroup()

  catch_by_yr[is.na(catch_by_yr)] <- 0

  catch_by_yr <- catch_by_yr |>
    mutate(total_weight = round(total_weight / catch_scale, digits))

  if(!write_file){
    return(catch_by_yr)
  }

  fn <- here(data_tables_path, catch_by_year_fn)
  write_csv(catch_by_yr, fn)
  message("The file:\n`", fn, "`\nwas written with new catch data\n")
}
