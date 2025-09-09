#' Plot catch data as a stacked barplot
#'
#' @param ct The data frame which is read in from `landings-tac-history.csv`
#' @param leg_font_size The legend font size
#' @param leg_key_size The size in cm of the bars of color in the legend
#' @param leg_ratio A vector of length two, for the ratio of the legend size
#' vs the plot size respectively. Default is c(1, 2). You will likely need to
#' change this if changing `leg_key_size` and/or `leg_font_size`
#' @param clip_cover There is a white rectangle drawn on top of the plot
#' to cover any of the plot that made it outside the plot area. `clip` has to
#' be set to `off` for the major x-axis tick marks to work, So, this is required.
#' If you make the plot in a grid, the rectangle may overwrite some of the plot
#' above it, and this number will have to be changed through trial and error
#' until you cannot see the white rectangle anymore.
#' @param xlim The year limits to plot
#' @param x_breaks The year value tick marks to show for the x axis
#' @param x_expansion Amount to expand the x axis. See the `expand` argument
#' in [ggplot2::scale_x_continuous()]
#' @param x_labs_mod Value for major X-axis tick marks. Every Nth tick
#' will be longer and have a label. The first and last will be shown
#' regardless of what this number is
#' @param ylim The y-axis min and max limits for the plot
#' @param y_breaks The tick mark values to show for the y axis
#' @param ret_df If `TRUE`, return a data frame of the catch by year. For
#' populating the `data-tables/landings-tac-history.csv` file
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catches <- function(ct,
                         leg_font_size = 12,
                         leg_key_size = 0.7,
                         clip_cover = 2,
                         fill_color = "royalblue",
                         xlim = c(1979, year(Sys.time()) - 1),
                         x_breaks = xlim[1]:xlim[2],
                         x_expansion = 1,
                         x_labs_mod = 5,
                         ylim = c(0, 12000),
                         y_breaks = seq(ylim[1], ylim[2], 1000),
                         ret_df = FALSE){

  yr_col <- sym(tr("Year"))
  catch_col <- sym(paste0(tr("Catch"), " (t)"))

  ct <- ct |>
    rename(!!yr_col := Year) |>
    rename(!!catch_col := `Catch (t)`) |>
    dplyr::filter(!!yr_col %in% xlim[1]:xlim[2])

  if(ret_df){
    return(ct)
  }

  x_labels <- make_major_tick_labels(x_breaks = x_breaks,
                                     modulo = x_labs_mod)

  g <- ggplot(ct,
              aes(x = !!yr_col,
                  y = !!catch_col)) +
    #fill = forcats::fct_reorder(fishery, ord))) +
    geom_hline(yintercept = y_breaks,
               linetype = "dashed",
               linewidth = 0.25) +
    geom_col(color = "transparent",
             fill = fill_color) +
    scale_x_continuous(expand = c(0, x_expansion),
                       breaks = x_breaks,
                       labels = x_labels) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = y_breaks,
                       labels = scales::comma) +
    coord_cartesian(xlim = xlim,
                    ylim = ylim,
                    clip = "off") +
    theme(legend.position = "none",
          # These two commands move the x-axis major tick labels and axis
          # title down so that the ticks. tick labels, and axis title don't
          # overlap each other
          axis.text.x = element_text(vjust = -2),
          axis.title.x = element_text(vjust = -2))

  # Add major tick marks
  g <- g |>
    add_major_ticks(x_breaks = x_breaks,
                    modulo = x_labs_mod,
                    # This proportion must be set by trial and error
                    # Make sure to change `vjust` value above in the `theme()`
                    # call so the labels are not overlapping the lines or
                    # too far away from the lines
                    prop = 1.2)

  g
}
