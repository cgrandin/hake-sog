#' Merge DMP and LOGS catch data and extract into a data frame
#'
#' @param lst a list of two data frames as returned by
#' [load_catch_data()]
#' @return A list of three data frames, one for each gear: Freezer trawlers,
#' Shoreside, and Joint Venture
#' @export
extract_catch <- function(lst){

  if(length(lst) != 2){
    stop("The length of the input list `lst` does not equal 2")
  }
  if(!all(names(lst) %in% c("dmp_df",
                            "logs_df"))){
    bail("The names of the elements in the list `lst` are not correct. ",
         "They must be `dmp_df`, and `logs_df`")
  }

  dmp_df <- lst$dmp_df
  logs_df <- lst$logs_df

  # At Sea Observer Program records only (all of those are discards in the
  # LOGS data), for all fleet types
  discards_df <- lst$logs_df |>
    dplyr::filter(source == "ASOP") |>
    dplyr::filter(!is.na(released_wt))

  summarize_dmp <- \(d, wt_col){

    wt_col_sym <- sym(wt_col)
    d |>
      complete(landing_date = seq.Date(min(landing_date),
                                       max(landing_date),
                                       by = "day")) |>
      mutate(year = year(landing_date),
             month = month(landing_date),
             day = day(landing_date),
             !!wt_col_sym := as.numeric(!!wt_col_sym)) |>
      select(year, month, day, !!wt_col_sym) |>
      group_by(year, month, day) |>
      summarize(landings = sum(!!wt_col_sym) * lbs_to_kilos,
                count =  n()) |>
      ungroup() |>
      mutate(landings = ifelse(is.na(landings), 0, landings))
  }

  dmp_summary <- dmp_df |>
    summarize_dmp("converted_wght_lbs_")
  discards_summary <- discards_df |>
    summarize_dmp("released_wt")

  join_dmp <- \(d, d_discards){

    full_join(d,
              d_discards,
              by = c("year", "month", "day")) |>
      group_by(year, month, day) |>
      mutate(landings.x = ifelse(is.na(landings.x),
                                 0,
                                 landings.x),
             landings.y = ifelse(is.na(landings.y),
                                 0,
                                 landings.y)) |>
      summarize(landings = landings.x + landings.y,
                landings_count = count.x,
                discards_count = count.y) |>
      ungroup() |>
      mutate(discards_count = ifelse(is.na(discards_count),
                                     0,
                                     discards_count)) |>
      arrange(year, month, day)
  }

  map2(list(dmp_summary), list(discards_summary),
       join_dmp)
}
