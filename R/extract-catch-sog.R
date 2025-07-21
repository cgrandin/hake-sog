#' Extract the Strait of Georgia catch by year for the SS3 input file
#'
#' @details
#' Cannot use `trp_type` here as you would expect. The trip types for the SoG
#' vary from the GULF hake (as expected) to "Option A Hake Shoreside" to `NA` so
#' we just extract by area and gear type to get midwater caught hake in
#' area 4B (excluding the Strait of Juan De Fuca). Landings and Discards are
#' added together.
#'
#' @param ct The output of [gfdata::get_catch()]
#'
#' @returns A data.frame of the catch by year for hake in the SoG
#' @export
extract_catch_sog <- \(ct){

  j <- ct |>
    dplyr::filter(major_stat_area_code == "01",
                  minor_stat_area_code != "20",
                  gear == "MIDWATER TRAWL") |>
    mutate(year = year(best_date)) |>
    group_by(year) |>
    summarize(landings = sum(landed_kg + discarded_kg) / 1000) |>
    ungroup()

  j
}

