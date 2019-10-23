#' Prepare data for scatter plot
#'
#' Pad out data if group and/or text are missing
#'
#' @param data data frame.
#' @param x x variable
#' @param y y variable
#' @param group Optional grouping vairiable
#' @param text Optional text variable to show in hover info
#' @param na.rm remove missing x/y observations.
#'

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
  data <- data.frame(
    x = data[[x]],
    y = data[[y]],
    group = group,
    text = text
  )

  # remove na data
  if (na.rm) {

    data <- tidyr::drop_na(data, x, y)

  }

  data

}

