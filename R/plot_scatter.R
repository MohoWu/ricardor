#' Scatter plot with plotly
#'
#' @param data Data frame.
#' @param x Name of x variable.
#' @param y Name of y variable.
#' @param group Optional name for grouping variable.
#' @param text Optional name for text variable. This will show in the hover text box.
#' @param hoverinfo If text variable is specified, use "x+y+text" to show text.
#'  For more info, check [plotly::schema()]
#' @param linear Whether a linear best fit line is drawn.
#' @param colors RColorBrewer palette name.
#' @param one2one_line Should the 1:1 line be drawn.
#' @param eq_x A number between 0 to 1 to specify the location of equation relative to the x axis.
#' @param eq_y A number between 0 to 1 to specify the location of equation relative to the y axis.
#' @param ... Other arguments passed to [plotly::plot_ly()]
#'
#' @return A plotly object.
#' @export
#'

plot_scatter <- function(data, x, y, group = NULL, text = NULL, hoverinfo = "x+y+text",
                         linear = TRUE, colors = "Set1", one2one_line = FALSE,
                         eq_x = 0.6, eq_y = 1, ...) {


  # helper function
  prep_data <- function(data, x, y, group, text, na.rm = TRUE) {

    # replace missing group with x_y name
    if (is.null(group)) {

      group <- paste(x, y, sep = "_")

    } else {

      group <- data[[group]]

    }

    # replace missing text with NA
    if (is.null(text)) {

      text <- NA_character_

    } else {

      text <- data[[text]]

    }

    # prepare data
    data_new <- data.frame(
      x = data[[x]],
      y = data[[y]],
      group = group,
      text = text
    )

    if ("id" %in% names(data)) {

      data_new$id = data[["id"]]

    }

    # remove na data
    if (na.rm) {

      data_new <- tidyr::drop_na(data_new, x, y)

    }

    data_new

  }

  # prepare data drop NAs
  dat <- prep_data(data, x, y, group, text)

  # plotting
  scatter <- dat %>%
    plotly::plot_ly(x = ~x, ...) %>%
    plotly::add_markers(y = ~y, color = ~group, hoverinfo = hoverinfo, text = ~text,
                        colors = colors) %>%
    layout(xaxis = list(title = x),
           yaxis = list(title = y))

  # calculate linear line if there are at least 2 data points
  if (linear && NROW(dat) >= 2 && !is.numeric(dat[["group"]])) {

    # separate lm model for each group
    fit <- dat %>%
      tidyr::nest(data = c(-group)) %>%
      mutate(model = purrr::map(data, ~lm(y ~ x, .x)))

    # create a fitted y data frame
    fitted_data <- fit %>%
      mutate(y_fitted = purrr::map(model, fitted)) %>%
      tidyr::unnest(cols = c(data, y_fitted))

    # prepare data frame for lm equations
    xmax <- max(dat$x)
    ymax <- max(dat$y)

    fitted_eqs <- fit %>%
      mutate(slope = purrr::map_dbl(model, ~round(coefficients(.x)["x"], 2)),
             intercept = purrr::map_dbl(model, ~round(coefficients(.x)["(Intercept)"]), 2),
             rsq = purrr::map_dbl(model, ~round(summary(.x)$r.squared, 2)),
             form = paste0("y = ", slope, "x ", ifelse(intercept < 0, "", "+"), intercept, ", r2 = ", rsq),
             x = xmax * eq_x,
             y = seq(ymax * eq_y,
                     by = sd(dat$y) * 0.5,
                     length.out = n()))

    scatter <- scatter %>%
      plotly::add_lines(y = ~y_fitted, x = ~x, color = ~group, data = fitted_data) %>%
      plotly::add_text(y = ~y, x = ~x, text = ~form, color = ~group, data = fitted_eqs,
                       showlegend = FALSE)

  }

  if (one2one_line) {

    df_line <- data.frame(x = c(0, max(data$x, na.rm = TRUE)),
                          y = c(0, max(data$x, na.rm = TRUE)))

    scatter <- scatter %>%
      plotly::add_lines(data = df_line, x = ~x, y = ~y,
                        name = "1:1 line", color = "blue")

  }

  # return
  scatter

}


