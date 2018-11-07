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







