#' Extract the Strait of Georgia survey samples by year for the SS3
#' input file
#'
#' @param d The output of [gfdata::get_survey_samples()]
#'
#' @returns A data.frame of the survey samples by year for hake in the SoG
#' @export
extract_survey_samples_sog <- \(d){

  j <- d |>
    dplyr::filter(survey_abbrev == "SOGERI")

  j
}

