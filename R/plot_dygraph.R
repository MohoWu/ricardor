#' Interactive time series
#'
#' This function plots an interactive time series using \code{dygraph} package.
#'
#' @param df A dataframe containing variables to plot in a wide format.
#' @param x Name of the variable to plot as x axis. Default is "date". Note that
#'  the this does not have to be a \code{Date} or \code{POSIXct} class variable.
#'  Regular numeric variable is also supported, such as year 2010, 2011, ...
#' @param vars The name of variables to plot. If not supplied, all the numeric
#'  variables will be plotted.
#' @param normalise Should the data be normalised. This is suitable for plotting
#'  variables of different scales. Default is \code{FALSE}.
#' @param dy.label This specify the column containing additional info to add the
#'  dygraph. Default is \code{NA}.
#' @param draw.points Draw points in the series? The default is \code{FALSE}.
#' @param point.size Default value is 2, which is generally appropriate. A value
#'  of 0.5 is very subtle.
#' @param line.width Default value is 0.5.
#' @param connect.points Force the line to be drawn between points with gap.
#'  Default is \code{TRUE}.
#' @param tz Time zone of the time series data.
#' @param dy.group This specifies the group of the dygraphs.
#' If multiple dygraphs are plotted and you want zooming in one of the graphs to be synced with other graphs,
#' then set all the dygraphs to the same group.
#' @param ylab Name of y axis.
#' @param ylim Set the vertical range of the graph to c(low, high). See \code{\link[dygraphs]{dyAxis}}.
#' @param ... other arguements passed to \code{\link[dygraphs]{dygraph}}.
#'
#' @examples
#'
#' @import dygraphs
#'
#' @export
#'


plot_dygraph <- function(df, x = "date", vars, normalise = FALSE, dy.label = NA, tz = "UTC",
                         draw.points = FALSE, point.size = 2, line.width = 0.5,
                         connect.points = TRUE,
                         highlight.individual = FALSE,
                         dy.group = NA, ylab = NULL, ylim = NULL, ...) {

  # the Data
  if (missing(vars)) {
    num_id <-  sapply(df, is.numeric)
    theData <-  df[num_id] %>%
      select(-one_of(x)) # exlcude the x var if x is also numeric

  } else {
    theData <- tryCatch(df[vars],
                        error = function(e) {
                          # if vars contains column names not in df,
                          # choose the ones exist in df
                          df[intersect(vars, names(df))]
                        })
  }

  # normalise data
  if (normalise) theData <- theData %>% mutate_all(funs(./mean(., na.rm = TRUE)))

  # the Date
  theDate <-  df[[x]]

  # create time series data
  ## check if x is date or numeric data
  if (is.numeric(theDate)) {

    ts <- cbind(theDate, theData)

  } else {

    ts <- xts::xts(theData, order.by = theDate, tz = tz)

  }

  # plot dygraph
  colour_vector <- ggplot2_colours(NCOL(theData))
  plot <- dygraph(ts, group = dy.group, ...) %>%
    dyOptions(colors = colour_vector,
              useDataTimezone = TRUE,
              drawPoints = draw.points,
              pointSize = point.size,
              strokeWidth = line.width,
              connectSeparatedPoints = connect.points) %>%
    dyAxis("y", label = ylab, valueRange = ylim) %>%
    dyLegend(width = 400) %>%
    dyRangeSelector()

  # additional label in the legend?
  if (!is.na(dy.label)) {
    plot <- plot %>%
      # add label to the hover text
      # taken from https://stackoverflow.com/questions/27671576/how-can-i-get-tooltips-showing-in-dygraphs-without-annotation
      dyCallbacks(
        highlightCallback = sprintf(
          'function(e, x, pts, row) {

          // added to illustrate what is happening
          //   remove once satisfied with your code
          debugger;

          var customLegend = %s


          // should get our htmlwidget
          var legendel = e.target.parentNode.parentNode
          .querySelectorAll(".dygraph-legend")[0];

          // should get our htmlwidget
          legendel.innerHTML = legendel.innerHTML + "<br>" + customLegend[row];
  }'
          ,# supply a vector or text that you would like
          jsonlite::toJSON(df[[dy.label]])
        )
      )
  }

  # Highlight individual time sereis?
  if (highlight.individual) {
    plot <- plot %>%
      dyLegend(width = 300) %>%

      # the following copied from https://stackoverflow.com/questions/35943583/plain-dygraphs-javascript-options-in-r-shiny
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
      dyCSS(textConnection("
     .dygraph-legend > span { display: none; }
     .dygraph-legend > span.highlight { display: inline; }
  "))
  }

  # return
  plot


}


#' Extract colour codes in ggplot2
#'
#' no export
#'
ggplot2_colours <- function (n = 2) {
  hues <- seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
