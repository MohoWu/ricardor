#' Shift variables forward or backward by some observations
#'
#' Sometimes we want to shift some measurements backward or forward.
#' [dplyr::lead()] and [dplyr::lag()] do this but will only operate on a single
#' variable. [var_shift()] extends this functionality and allows shifting multiple
#' variables by a range of steps.
#'
#' @param df Data frame containing variables to shift.
#' @param vars Variable(s) to shift.
#' @param n Number of observations to shift.
#' @param fn The shifting function. Usually this is [dplyr::lead()] or [dplyr::lag()].
#' This could also be a custom function that takes a vector and a number `n`.
#'
#' @return Data frame with append shifted measurements.
#' @export
#'
#' @examples
#' \dontrun{
#' shift(mtcars, c("vs", "am"), lead, 1:2)
#' }

var_shift <- function(df, vars, n, fn) {

  fn_name <- quo_name(enquo(fn))

  df <- df %>%
    select(vars) %>%
    purrr::imap_dfc(~purrr::set_names(purrr::map(n, fn, x = .x),
                                      paste(.y, fn_name, n, sep = '_'))) %>%
    bind_cols(df, .)

  # return
  df

}
