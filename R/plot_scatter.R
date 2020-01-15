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
#' @param colors RColorBrewer pallete name.
#'
#' @return A plotly object.
#' @export
#'

plot_scatter <- function(data, x, y, group = NULL, text = NULL, hoverinfo = "x+y",
                         linear = TRUE, colors = "Set3") {

  # prepare data drop NAs
  data <- prep_data(data, x, y, group, text)

  # plotting
  scatter <- data %>%
    plotly::plot_ly(x = ~x) %>%
    plotly::add_markers(y = ~y, color = ~group, hoverinfo = hoverinfo, text = ~text,
                        colors = colors)

  # calculate linear line if there are at least 2 data points
  if (linear && NROW(data) >= 2) {

    # separate lm model for each group
    fit <- data %>%
      group_by(group) %>%
      group_map(~lm(y ~ x, data = .))

    names(fit) <- unique(data$group)

    # prepare data frame for the fitted lines
    fitted_data <- data.frame(
      y = purrr::map(fit, fitted) %>% unlist(),
      x = data$x,
      group = data$group
    )

    # prepare data frame for lm equations
    fitted_eqs <- fit %>%
      purrr::map_df(~as.data.frame(rbind(round(coef(.x), 2))), .id = "group") %>%
      rename(intercept = `(Intercept)`) %>%
      mutate(
        rsq = purrr::map_dbl(fit, ~round(summary(.x)$r.squared, 3)),
        form = paste0("y = ", x, "x ", ifelse(intercept < 0, "", "+"), intercept, ", r2 = ", rsq),
        x = quantile(data$x, 0.1),
        y = seq(max(data$y),
                by = sd(data$y) * 0.5,
                length.out = n()))


    scatter <- scatter %>%
      plotly::add_lines(y = ~y, x = ~x, color = ~group, data = fitted_data) %>%
      plotly::add_text(y = ~y, x = ~x, text = ~form, color = ~group, data = fitted_eqs,
                       showlegend = FALSE)

  }

  # return
  scatter

}
