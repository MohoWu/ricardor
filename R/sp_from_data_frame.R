#' Function to promoting a data frame to a SpatialPointsDataFrame,
#' SpatialLinesDataFrame, or SpatialPolygonsDataFrame.
#'
#' @param df Data frame to be converted into spatial data frame.
#'
#' @param type Type of geomerty. Type must be one of `"points"`,
#' `"lines"`, or `"polygons"` and there is no default.
#'
#' @param latitude `df`'s latitude variable name.
#'
#' @param longitude `df`'s longitude variable name.
#'
#' @param projection `df`'s latitude and longitude projection system.
#' Default is WGS84.
#'
#' @param keep For when type is `"points"`, should latitude and longitude
#' be kept in the SpatialPointsDataFrame's data slot?
#'
#' @param id Variable in `df` which codes for spatial object's id. This is
#' used when a data frame contains many seperate geometries. `id` is not
#' used for points because each point is a seperate geometry.
#'
#' @author Stuart K. Grange
#'
#' @return A SpatialPointsDataFrame, SpatialLinesDataFrame, or
#' SpatialPolygonsDataFrame.
#'
#'
#' @examples
#' \dontrun{
#'
#' # Promote to different geometry types
#' # Points
#' sp_points <- sp_from_data_frame(data_drawn, type = "points")
#'
#' # Lines
#' sp_lines <- sp_from_data_frame(data_drawn, type = "lines")
#' # Lines with seperate geometries
#' sp_lines <- sp_from_data_frame(data_drawn, type = "lines", id = "name")
#'
#' # Polygons
#' sp_polygons <- sp_from_data_frame(data_drawn, type = "polygons")
#' # Polygons with seperate geometries
#' sp_polygons <- sp_from_data_frame(data_drawn, type = "polygons", id = "name")
#'
#' }
#'
#' @export
sp_from_data_frame <- function(df, type, latitude = "latitude",
                               longitude = "longitude",
                               projection = projection_wgs84(),
                               keep = FALSE, id = NA) {

  #  Check and parse
  if (length(type) != 1) stop("'type' must have a length of one. ", call. = FALSE)

  # Make plurals
  type <- stringr::str_trim(type)
  type <- stringr::str_to_lower(type)
  type <- ifelse(type == "point", "points", type)
  type <- ifelse(type == "line", "lines", type)
  type <- ifelse(type == "polygon", "polygons", type)

  # Check
  if (!grepl("points|lines|polygons", type))
    stop("'type' must be one of 'points', 'lines', or 'polygons'.", call. = FALSE)

  if (any(is.na(df[latitude])) || any(is.na(df[longitude]))) {

    warning("Missing latitude and longitude detected and removed.", call. = FALSE)

    df <- df[complete.cases(df[c(latitude, longitude)]), ]

  }

  # Promote to spatial data
  if (type == "points")
    sp <- data_frame_to_points(df, latitude, longitude, projection, keep)

  if (type == "lines")
    sp <- data_frame_to_lines(df, latitude, longitude, projection, id)

  if (type == "polygons")
    sp <- data_frame_to_polygons(df, latitude, longitude, projection, id)

  return(sp)

}


data_frame_to_points <- function(df, latitude, longitude, projection,
                                 keep = FALSE) {

  # NA check, if NAs drop them
  if (any(is.na(c(df[, latitude], df[, longitude])))) {

    # Remove NAs
    df <- df[!(is.na(df[, latitude]) | is.na(df[, longitude])), ]

    # Raise a warning
    warning(
      "Missing coordinates were detected and have been removed...",
      call. = FALSE
    )

    # Check
    if (nrow(df) == 0) stop("There are no valid coordinates...", call. = FALSE)

  }

  # Reassign, df can be used again
  sp <- df

  # Make sp points object
  sp::coordinates(sp) <- c(longitude, latitude)

  # Put vectors back in data slot
  if (keep) {

    sp@data[, latitude] <- df[, latitude]
    sp@data[, longitude] <- df[, longitude]

  }

  # Give the object a projection
  if (!is.na(projection)) sp <- sp_transform(sp, projection, warn = FALSE)

  return(sp)

}


data_frame_to_lines <- function(df, latitude, longitude, projection, id) {

  # Make an identifier variable for lines
  if (is.na(id)) {

    # Single line object, no grouping
    df[, "id"] <- 1

  } else {

    # Use input variable
    df[, "id"] <- df[, id]

  }

  # Get data part for the SpatialLinesDataFrame
  data_extras <- dplyr::distinct(df, id, .keep_all = TRUE)

  # Drop the coordinates from extra data frame
  data_extras[, c(latitude, longitude)] <- NULL

  # Make sp points object
  sp_object <- data_frame_to_points(df, latitude, longitude, projection)

  # From
  # http://stackoverflow.com/questions/24284356/convert-spatialpointsdataframe-
  # to-spatiallinesdataframe-in-r
  # Generate lines for each id
  lines <- lapply(
    split(sp_object, sp_object$id),
    function(x) sp::Lines(list(Line(sp::coordinates(x))), x$id[1L])
  )

  # Drop
  if (!is.na(id)) data_extras[, "id"] <- NULL

  # Create SpatialLines
  sp <- sp::SpatialLines(lines)

  # Make SpatialLinesDataFrame
  sp <- sp::SpatialLinesDataFrame(sp, data_extras, match.ID = FALSE)

  # Give the object a projection
  if (!is.na(projection)) sp <- sp_transform(sp, projection, warn = FALSE)

  return(sp)

}


data_frame_to_polygons <- function(df, latitude, longitude, projection, id) {

  # Make an identifier variable for lines
  if (is.na(id)) {

    # Single line object, no grouping
    df[, "id"] <- 1

  } else {

    # Use input variable
    df[, "id"] <- df[, id]

  }

  # Get data part for the SpatialLinesDataFrame
  data_extras <- dplyr::distinct(df, id, .keep_all = TRUE)

  # Drop the coordinates from extra data frame
  data_extras[, c(latitude, longitude)] <- NULL

  # Make sp points object
  sp <- data_frame_to_points(df, latitude, longitude, projection)

  # A list element will represent each group within a feature
  # Long-lat order is important
  coordinates <- plyr::dlply(
    df,
    "id",
    function(x)
      data.matrix(x[, c(longitude, latitude)])
  )

  # Make polygons
  sp <- lapply(
    seq_along(coordinates),
    function(x)
      matrix_to_sp_polygon(coordinates[x], x)
  )

  # Bind geometries
  sp <- do.call(rbind, sp)

  # Make sp dataframe
  sp <- sp::SpatialPolygonsDataFrame(sp, data_extras)

  # Give the object a projection
  if (!is.na(projection)) sp <- sp_transform(sp, projection, warn = FALSE)

  return(sp)

}


# No export
matrix_to_sp_polygon <- function(matrix, id) {

  # Matix to polygon
  polygon <- sp::Polygon(matrix)

  # Polygon to polygons
  polygon <- sp::Polygons(list(polygon), id)

  # To spatial polygons
  sp <- sp::SpatialPolygons(list(polygon))

  return(sp)

}
