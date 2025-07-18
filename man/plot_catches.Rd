% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-catches.R
\name{plot_catches}
\alias{plot_catches}
\title{Plot catch data as a stacked barplot}
\usage{
plot_catches(
  ct,
  type = c("Landed", "Discarded"),
  leg_font_size = 12,
  leg_key_size = 0.7,
  clip_cover = 2,
  fill_color = "blue",
  xlim = c(1979, year(Sys.time()) - 1),
  x_breaks = xlim[1]:xlim[2],
  x_expansion = 1,
  x_labs_mod = 5,
  ylim = c(0, 12),
  y_breaks = seq(ylim[1], ylim[2], 1)
)
}
\arguments{
\item{ct}{The data frame which is read in from \code{landings-tac-history.csv}}

\item{leg_font_size}{The legend font size}

\item{leg_key_size}{The size in cm of the bars of color in the legend}

\item{clip_cover}{There is a white rectangle drawn on top of the plot
to cover any of the plot that made it outside the plot area. \code{clip} has to
be set to \code{off} for the major x-axis tick marks to work, So, this is required.
If you make the plot in a grid, the rectangle may overwrite some of the plot
above it, and this number will have to be changed through trial and error
until you cannot see the white rectangle anymore.}

\item{xlim}{The year limits to plot}

\item{x_breaks}{The year value tick marks to show for the x axis}

\item{x_expansion}{Amount to expand the x axis. See the \code{expand} argument
in \code{\link[ggplot2:scale_continuous]{ggplot2::scale_x_continuous()}}}

\item{x_labs_mod}{Value for major X-axis tick marks. Every Nth tick
will be longer and have a label. The first and last will be shown
regardless of what this number is}

\item{ylim}{The y-axis min and max limits for the plot}

\item{y_breaks}{The tick mark values to show for the y axis}

\item{leg_ratio}{A vector of length two, for the ratio of the legend size
vs the plot size respectively. Default is c(1, 2). You will likely need to
change this if changing \code{leg_key_size} and/or \code{leg_font_size}}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object
}
\description{
Plot catch data as a stacked barplot
}
