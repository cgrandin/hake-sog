#' Extract the Strait of Georgia commercial samples by year for the SS3
#' input file
#'
#' @details
#' Cannot use `trp_type` here as you would expect. The trip types for the SoG
#' vary from the GULF hake (as expected) to "Option A Hake Shoreside" to `NA` so
#' we just extract by area and gear type to get midwater caught hake in
#' area 4B (excluding the Strait of Juan De Fuca). Landings and Discards are
#' added together.
#'
#' @param d The output of [gfdata::get_commercial_samples()]
#'
#' @returns A data.frame of the commercial samples by year for hake in the SoG
#' @export
extract_commercial_samples_sog <- \(d){

  j <- d |>
    dplyr::filter(major_stat_area_code == "01",
                  !minor_stat_area_code %in% c("12", "20"),
                  gear_desc == "MIDWATER TRAWL")

  j
}

