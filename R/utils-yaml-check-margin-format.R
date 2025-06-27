#' Check the format of the margin command entered into the YAML header.
#'
#' @param fig_margins The string containing the command to verify
#' @param index_fn The index RMD file name. `index.Rmd` by default
#'
#' @return [invisible()] if format is correct, or stops if the format is
#' incorrect
#' @export
yaml_check_margin_format <- function(fig_margins,
                                     index_fn = "index.Rmd"){

  margin_format_ok <-
    length(grep("^margin\\(\\d+, *\\d+, *\\d+, \\d+\\) *#?.*$",
                fig_margins))

  if(!margin_format_ok){
    bail("`figure_margin` in ", index_fn, " is not in the correct format. ",
         "It should be formatted this way:\nmargin(A, B, C, D) where A, B, C, ",
         "and D are integers representing the top, right, bottom, and left ",
         "margin sizes for all figures in the document")
  }

  invisible()
}
