% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_calendar.R
\name{plot_calendar}
\alias{plot_calendar}
\title{ggplot2 version of calendar plot}
\usage{
plot_calendar(data, fill, wd = NULL, ws = NULL, text = wd, draw.arrow = TRUE)
}
\arguments{
\item{data}{Data frame containing a \code{date} column}

\item{fill}{The column to fill the calendar plot. This is usually the pollutant
concentration.}

\item{wd, ws}{Optional wind speed and wind direction plotted as arrows.}

\item{text}{Optional column to pass to \code{text} in \code{\link[ggplot2:aes]{ggplot2::aes()}}. This can be
useful if you intend to convert to plotly and control what is showing up
when you hover over the plot.}

\item{draw.arrow}{Should the wind vector arrows be drawn. This is better turned off
if the plot is going to be converted to plotly.}
}
\description{
A ggplot2 equivalent of \code{\link[openair:calendarPlot]{openair::calendarPlot()}}
}
\details{
Note that the ggplot2 object returned will not have any scale
defined for the fill, so that they can be added easily with an ad-hoc approach.
}
\note{
If the dates span multiple years, the year will be appended
to the month name automatically, otherwise, it will not appear.
}
\examples{
{


}
}
