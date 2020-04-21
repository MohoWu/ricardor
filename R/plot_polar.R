#' Polar scatter plot
#'
#' @param data A data frame containing wind speed and wind direction data with names
#'  `ws` and `wd`, respectively.
#' @param pol The name of the pollutant to plot.
#' @param popup Column(s) to show in the hoverbox. Default is the pol concentration.
#' @param ... Additional arguements passed to [plotly::plot_ly()]
#'
#' @return A plotly object
#' @importFrom plotly plot_ly add_trace layout
#' @export
#'
#' @examples
#' \dontrun{
#' plot_polar(data, "NO2")
#' }
#'
#'

plot_polar <- function(data, pol, popup = pol, ...) {

  # remove NA data
  data <- na.omit(data[c(popup, pol, "ws", "wd")])

  # popup string
  popup <- create_popup_string(data, popup)

  # no x and y axes labels
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )

  # polar plot
  plot_ly(
    data = data,
    type = 'scatterpolar',
    mode = "markers",
    hoverinfo = "text",
    text = popup,
    ...
  ) %>%
    add_trace(
      name = pol,
      r = ~ws,
      theta = ~wd,
      color = data[[pol]]
    ) %>%
    layout(
      polar = list(
        radialaxis = list(
          title = "wind speed",
          tickfont = list(
            size = 8
          )
        ),
        angularaxis = list(
          tickfont = list(
            size = 8
          ),
          rotation = 90,
          direction = 'clockwise'
        )
      ),
      xaxis = ax,
      yaxis = ax,
      showlegend = FALSE
    )

}
