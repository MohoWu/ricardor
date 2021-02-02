#' Convert all characters to factors in a data frame
#'
#' @param df Data frame.
#'
#' @export
#'

factor_df <- function(df) {

  df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],
                                         as.factor)

  df
}



#' Generate n distinct colours
#'
#' @param n Number of colours to generate
#'
#' @return A vector of colour names.
#' @export
#'

distinct_colours <- function(n) {

  # https://stackoverflow.com/a/33144808/4227151
  colours <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]

  set.seed(123)
  sample(colours, n)
}



