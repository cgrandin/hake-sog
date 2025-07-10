#' create the weight-at-age CSV file for Canadian commercial data
#'
#' @details
#' No filtering is performed in this function.
#' Creates a CSV file containing the commercial weight-at-age
#'
#' @param d A list as returned by [gfdata::get_commercial_samples()]
#' @param weight_scale A value to divide the weights by
#' @param num_sex Either 1 or 2. If 1, then fill the column `Sex` with `U`
#' for every record. If 2, use the data to fill the `Sex` column with
#' `M`, `F`, or `U` depending on input values
#' @param ret_df Logical. If `TRUE`, return the weight-at-age data frame.
#' If `FALSE`, write the data frame to the Canadian weight-at-age CSV file
#'
#' @return a data frame with `Source` set to `CAN_freezer`, `CAN_shoreside`,
#' `CAN_jv`, and `CAN_polish` for the four fishery types

#' @export
create_commercial_waa <- function(d,
                                  weight_scale = 1e3,
                                  num_sex = 2,
                                  ret_df = FALSE){

  if(!num_sex %in% 1:2){
    stop("`num_sex` must be either 1 to set all rows' sex value to `U` ",
         "or 2 to use the sex data and recode it to `M` and `F`")
  }

  df <- d |>
    dplyr::filter(trip_sub_type_desc %in%
                    c("OBSERVED DOMESTIC",
                      "NON - OBSERVED DOMESTIC"))

    if(num_sex == 1){
      get_sex_code <- \(sex){"U"}
    }else{
      get_sex_code <- \(sex){
        ifelse(is.na(sex), "U", ifelse(sex == 1, "M", "F"))
      }
    }
    wa_df <- df |>
      transmute(Weight_kg = weight / weight_scale,
                Sex = get_sex_code(sex),
                Age_yrs = age,
                Month = month(trip_start_date),
                Year = year(trip_start_date)) |>
      dplyr::filter(!if_any(everything(), is.na)) |>
      select(Year, Month, Sex, Age_yrs, Weight_kg)

  if(ret_df){
    return(wa_df)
  }

  fn <- here(data_tables_path, waa_fn)
  write_csv(wa_df, fn)
  message("The file:\n`", fn, "`\nwas written with new weight-at-age data\n")
  invisible()
}
