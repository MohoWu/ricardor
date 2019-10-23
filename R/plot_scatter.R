#' Scatter plot iwth plotly
#'
#' @param data Data frame.
#' @param x Name of x variable.
#' @param y Name of y variable.
#' @param group Optional name for grouping variable.
#' @param text Optional name for text variable. This will show in the hover text box.
#' @param hoverinfo If text variable is specified, use "x+y+text" to show text.
#'  For more info, check [plotly::schema()]
#' @param linear Whether a linear best fit line is drawn.
#'
#' @return A plotly object.
#' @export
#'

plot_scatter <- function(data, x, y, group = NULL, text = NULL, hoverinfo = "x+y", linear = TRUE) {

  data <- prep_data(data, x, y, group, text)

  scatter <- data %>%
    plotly::plot_ly(x = ~x) %>%
    plotly::add_markers(y = ~y, color = ~group, hoverinfo = hoverinfo, text = ~text,
                        colors = "Accent")


  if (linear) {

    fit <- lm(as.formula(paste("y", "x", sep = "~")), data)

    scatter <- scatter %>%
      plotly::add_lines(y = fitted(fit), name = "bestfit", color = I("darkred"))

  }

  scatter

}
