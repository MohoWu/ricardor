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


create_popup_string <- function(data, cols) {

  # Select variables, no comma to keep data frame structure
  data <- data[cols]

  # Get variable names
  names <- names(data)

  # Give names column-wise
  popup_string <- apply(data, 1, collapse_values_with_name, name = names)

  # Collapse row-wise, now just a vector
  if (class(popup_string) == "matrix")
    popup_string <- apply(popup_string, 2, stringr::str_c, collapse = "<br>")

  # No names for the vector
  popup_string <- unname(popup_string)

  # Return
  popup_string

}

collapse_values_with_name <- function(x, name, sep = ": ")
  stringr::str_c(name, sep, x)
