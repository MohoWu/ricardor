#' Function to calculate distances between two points in metres or kilometres.
#'
#' \code{distance_by_haversine} is used when distances between latitude and
#' longitude-pairs need to be represented in metre or kilometre units. The
#' distances between two points is based on the Haversine Formula which is
#' approximate and is most appropriate when used for short-distance calculations.
#'
#' The Haversine Formula does not compensate for Earth's non-spherical shape,
#' will overestimate distances in the polar regions but will underestimate
#' distances near the equator. For more information see
#' \url{https://en.wikipedia.org/wiki/Haversine_formula}
#'
#' \code{distance_by_haversine} is not a spatial function and does not have any
#' geographic library dependencies.
#'
#' @param longitude A vector of longitude values in decimal degrees.
#'
#' @param latitude A vector of latitude values in decimal degrees.
#'
#' @param longitude_lag A vector of longitude values in decimal degrees.
#'
#' @param latitude_lag A vector of latitude values in decimal degrees.
#'
#' @param unit The unit of the returned distance values. Can be metres or
#' kilometres. Metres is the default.
#'
#' @param radius The radius of the Earth in kilometres. The default value is
#' 6371, but I have seen versions of similar functions which use slightly
#' different values (plus or minus two or three kilometres) so this value can
#' be altered.
#'
#' @author Stuart K. Grange
#'
#'
#' @examples
#' \dontrun{
#' # What is the distance between Oxford's Radcliffe Camera and Cliveden House
#' in Buckinghamshire?
#' # In metres
#' distance_by_haversine(51.753447, -1.254024, 51.558170, -0.688250)
#' 44662.57
#'
#' # In kilometres
#' distance_by_haversine(51.753447, -1.254024, 51.558170, -0.688250, "km")
#' 44.66257
#'
#' # or because we are in a locale with a different distance unit, in miles
#' distance_by_haversine(51.753447, -1.254024, 51.558170, -0.688250, "km") * 0.62
#' 27.69079
#'
#'
#' # Data frame usage
#' # Distances of many air quality monitoring sites from the City of London
#' data_air$distance <- distance_by_haversine(
#'   data_air$latitude, data_air$longitude, 51.513468, -0.089133, "km")
#'
#' # Arrange by distance, closest at the top
#' data_air <- plyr::arrange(data_air, distance)
#'
#' }
#'
#' @export
distance_by_haversine <- function(latitude, longitude, latitude_lag = NA,
                                  longitude_lag = NA, unit = "metres",
                                  radius = 6371) {
  # Switch units, a check
  unit <- switch(unit,
    "m" =, "meter" =, "metre" =, "metres" =, "meters" = "meters",
    "km" =, "kilometer" =, "kilometre" =, "kilometers" =, "kilometres" = "kilometres")

  # Create lagged variables if not declared
  if (is.na(latitude_lag) & is.na(longitude_lag)) {

    latitude_lag <- dplyr::lag(latitude, 1)
    longitude_lag <- dplyr::lag(longitude, 1)

  }

  # Get degree deltas for coordinate pairs
  delta_longitude <- (longitude_lag - longitude) * pi / 180
  delta_latitude <- (latitude_lag - latitude) * pi / 180

  # Use the haversine function
  haversin <- sin(delta_latitude / 2) * sin(delta_latitude / 2) +
    cos(latitude * pi / 180) * cos(latitude_lag * pi / 180) *
    sin(delta_longitude / 2) * sin(delta_longitude / 2)

  # Calculate distance
  distance <- 2 * atan2(sqrt(haversin), sqrt(1 - haversin))

  # Calculate distance as metres
  distance <- radius * distance

  if (unit == "meters") distance <- distance * 1000

  # Return
  distance

}
