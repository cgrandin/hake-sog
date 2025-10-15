#' Title
#'
#' @param d
#' @param font_size
#' @param bold_headers
#' @param caption
#'
#' @returns
#' @export
#'
#' @examples
table_calibration <- \(d,
                       font_size = 9,
                       header_font_size = 10,
                       header_vert_spacing = 12,
                       header_vert_scale = 1.2,
                       ...){

  # Insert custom header fontsize before linebreaker
  if(is.null(header_font_size)){
    header_font_size <- font_size
  }
  hdr_font_str <- create_fontsize_str(header_font_size,
                                      header_vert_spacing,
                                      header_vert_scale)

  col_names <- names(d)
  col_names <- gsub("\\\\n", "\n", col_names)

  col_names <- gsub("\\n", paste0("\n", hdr_font_str$quad), col_names)
  col_names <- paste0(hdr_font_str$dbl, col_names)
  # Add \\makecell{} latex command to headers with newlines
  col_names <- linebreaker(col_names, align = "c")

  out <- csas_table(d,
                    format = "latex",
                    booktabs = TRUE,
                    linesep = "",
                    align = rep("r", ncol(d)),
                    col.names = col_names,
                    col_names_align = rep("r", ncol(d)),
                    ...)

  # if(!is.null(col_widths)){
  #   d <- d |>
  #     column_spec(2:ncol(d), width = col_widths)
  # }

  out
}
