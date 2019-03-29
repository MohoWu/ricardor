#' ggplot2 version of calendar plot
#'
#' A ggplot2 equivalent of [openair::calendarPlot()]
#'
#' Note that the ggplot2 object returned will not have any scale
#' defined for the fill, so that they can be added easily with an ad-hoc approach.
#'
#'
#' @param data Data frame containing a `date` column
#' @param fill The column to fill the calendar plot. This is usually the pollutant
#'   concentration.
#' @param wd,ws Optional wind speed and wind direction plotted as arrows.
#' @param text Optional column to pass to `text` in [ggplot2::aes()]. This can be
#'   useful if you intend to convert to plotly and control what is showing up
#'   when you hover over the plot.
#' @param draw.arrow Should the wind vector arrows be drawn. This is better turned off
#'  if the plot is going to be converted to plotly.
#'
#' @note If the dates span multiple years, the year will be appended
#' to the month name automatically, otherwise, it will not appear.
#'
#' @import ggplot2
#' @importFrom lubridate wday month year day "day<-"
#' @importFrom forcats fct_inorder
#'
#' @export
#'
#' @examples {
#'
#'
#' }
plot_calendar <- function(data, fill, wd = NULL, ws = NULL, text = NULL, draw.arrow = TRUE) {

  # substitute arguements
  fill <- enquo(fill)
  wd <- enquo(wd)
  ws <- enquo(ws)
  text <- enquo(text)
  text_name <- quo_name(text)

  # function to get the day of first day of a month
  first_day_of_month_wday <- function(dx) {
    day(dx) <- 1
    wday(dx)
  }

  # add all the variables required for a calendar plot
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
  if (quo_name(wd) != "NULL" && quo_name(ws) != "NULL") {
    gplot <- ggplot(mydata,
                    aes(dow, y, fill = !!fill, text = paste0(text_name, ": ", !!text))) +
      geom_tile(color="gray80", alpha = 0.75) +
      geom_text(aes(label = day, colour = !!ws),
                size = 2) +
      scale_color_gradientn(colours = openair::openColours("Blues"),
                            guide = FALSE)
    if (draw.arrow) {
      gplot <- gplot +
        geom_spoke(aes(angle = !!wd),
                   radius = 0.4,
                   arrow = arrow(length = unit(0.1, "cm")))
    }

  } else {
    gplot <- ggplot(mydata, aes(dow, y, fill = !!fill)) +
      geom_text(aes(label = day), size = 2) +
      geom_tile(color="gray80", alpha = 0.75)
  }

  # add to the plot
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
