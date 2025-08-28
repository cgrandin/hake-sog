#' Plot the density histogram for the posteriors for catchability for
#' the acoustic age-2+ survey
#'
#' @param model The base model
#' @param num_bins The number of bins to use for the histogram created using
#' [ggplot2::geom_histogram()]
#' @param x_breaks A vector of values to show on the x-axis
#' @param y_breaks A vector of values to show on the y-axis
#' @param bar_outline_color The color of the outline of each bar in the histogram
#' @param bar_fill The color of the fill for each bar in the histogram
#' @param bar_alpha The amount of transparency for each bar in the histogram
#' @param line_color The line color to use for the median line
#' @param line_type The line type to use for the median line
#' @param line_width  The line width to use for the median line
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_catchability_density <- function(model,
                                      num_bins = 30,
                                      x_breaks = seq(0, 1.2, 0.1),
                                      y_breaks = seq(0, 3, 0.5),
                                      bar_outline_color = "black",
                                      bar_fill = main_fill,
                                      bar_alpha = main_alpha,
                                      line_color = "green",
                                      line_type = "solid",
                                      line_width = 1){

  qvec <- model$extra_mcmc$q_vector
  set.seed(42)
  # For testing:
  # qvec <- rnorm(1000, 0.8, 0.2)
  qmed <- median(qvec)
  q <- qvec |>
    as_tibble() |>
    setNames("value")

  g <- ggplot(q) +
    geom_histogram(data = q,
                   mapping = aes(value, after_stat(density)),
                   fill = bar_fill,
                   alpha = bar_alpha,
                   col = bar_outline_color,
                   bins = num_bins) +
    geom_vline(data = q,
               aes(xintercept = !!qmed),
               linetype = line_type,
               linewidth = line_width * 1.5,
               color = "white") +
    geom_vline(data = q,
               aes(xintercept = qmed),
               linetype = line_type,
               linewidth = line_width,
               color = line_color) +
    scale_x_continuous(breaks = x_breaks,
                       limits = c(min(x_breaks), max(x_breaks)),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = y_breaks,
                       limits = c(min(y_breaks), NA),
                       expand = c(0, 0)) +
    xlab("") +
    ylab("")

 g
}
