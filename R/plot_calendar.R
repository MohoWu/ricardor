#' ggplot2 version of calendar plot
#'
#' A ggplot2 equivalent of \code{\link[openair]{calendarPlot}}
#'
#' Note that the ggplot2 object returned will not have any scale
#' defined for the fill, so that can be added to the returned
#' ggplot2 object.
#'
#'
#' @param data Data frame containing a \code{date} column
#' @param fill The column to fill the calendar plot.
#' @param wd,ws Optional wind speed and wind direction plotted as arrows.
#' @param draw.arrow Should the wind vector arrows be drawn. This is better turned off
#'  if the plot is going to be converted to plotly.
#'
#' @note If the dates span multiple years, the year will be appended
#' to the month name automatically, otherwise, it will not appear.
#'
#' @import ggplot2
#' @importFrom lubridate wday month year day
#' @importFrom forcats fct_inorder
#'
#' @export
#'
#' @examples {
#'
#'
#' }
plot_calendar <- function(data, fill, wd, ws, draw.arrow = TRUE) {

  # substitute arguements
  fill <- enquo(fill)
  fill_str <- quo_name(fill)

  if (!missing(wd) && !missing(ws)) {
    wd <- enquo(wd)
    wd_str <- quo_name(wd)
    ws <- enquo(ws)
    ws_str <- quo_name(ws)
  }


  first_day_of_month_wday <- function(dx) {
    day(dx) <- 1
    wday(dx)
  }

  mydata <- data %>%
    ungroup() %>%
    mutate(dow = wday(date),
           day = day(date),
           month = month(date, label = TRUE),
           year = year(date),
           monlabel = paste(month, year),
           monlabel = forcats::fct_inorder(factor(monlabel, ordered = TRUE)),
           fdom = first_day_of_month_wday(date),
           y = ceiling((day + fdom - 1) / 7))


  weekdays <- c("M", "T", "W", "T", "F", "S", "S")

  # put this in front since need to include all aes in the ggplot
  # in order to show full info when converting to plotly
  if (!missing(wd) && !missing(ws)) {
    gplot <- ggplot(mydata,
                    aes_string("dow", "y", fill = fill_str)) +
      geom_tile(color="gray80", alpha = 0.75) +
      geom_text(aes_string(label = "day", colour = ws_str),
                size = 2) +
      scale_color_gradientn(colours = openair::openColours("Blues"),
                            guide = FALSE)
    if (draw.arrow) {
      gplot <- gplot +
        geom_spoke(aes_string(angle = wd_str),
                   radius = 0.4,
                   arrow = arrow(length = unit(0.1, "cm")))
    }

  } else {
    gplot <- ggplot(mydata, aes_string("dow", "y",fill = fill_str)) +
      geom_text(aes(label = day), size = 2) +
      geom_tile(color="gray80", alpha = 0.75)
  }

  gplot <- gplot +
    facet_wrap(~monlabel, ncol = 3) +
    scale_x_continuous(expand=c(0,0),
                       position="top",
                       breaks=seq(1,7),
                       labels=weekdays,
                       sec.axis = dup_axis()) +
    scale_y_reverse(expand=c(0,0)) +
    scale_fill_gradientn(colours = openair::openColours("heat")) +
    coord_fixed() +
    theme(panel.background=element_rect(fill=NA, color=NA),
          strip.background = element_rect(fill=NA, color=NA),
          strip.text.x = element_text(hjust=0, face="bold"),
          legend.title = element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          axis.text.y = element_blank(),
          strip.placement = "outsite")

  # return
  gplot

}
