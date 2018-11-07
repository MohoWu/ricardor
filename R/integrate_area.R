#' Integrate area
#'
#' Integrate the area under a xy plot. Credit to [StackOverflow](https://stackoverflow.com/a/4955118/4227151)
#' The method used here is to sum the area of mutiple trapeziums under a line.
#'
#' @param df Data frame containing `x` and `y` of a plot.
#' @param x Column name for the x axis.
#' @param y Column name for the y axis.
#' @param skip.na Logical. How to treat `NA`s in `y`. If `TRUE` the trapezium of
#'  the area adjacent to the missing value will be calculated. If `FALSE` the
#'  two trapeziums before and after the missing value will be `NA`.
#'
#' @return A value of the area.
#' @export
#'

integrate_area <- function(df, x, y, skip.na = TRUE) {

  x <- df[[x]]
  # x should contains no NA
  if (any(is.na(x))) stop("x shouldn't contain any NA.", call. = FALSE)

  y <- df[[y]]

  if (skip.na) {
    # remove NA values in y
    id <- which(!is.na(y))

  } else id <- order(x)

  AUC <- sum(diff(x[id])*zoo::rollmean(y[id],2), na.rm = TRUE)

  # return the area
  AUC
}



