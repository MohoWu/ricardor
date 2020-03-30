#' Convenience function to quickly plot geometries on a leaflet map.
#'
#' @param data Spatial object or a data frame with coordinates
#'
#' @param popup Vector of variables to be used as a popup on the map.
#'
#' @param force Should the projection be forced to WGS84? Default is `TRUE`.
#'
#' @param colour Colour of geometry.
#'
#' @param col_group If the colour mapped on a numeric vector, how should the colours be grouped.
#'  Default is `NULL`. Can be "bin" or "quantile".
#'
#' @param n If the `col_group`` is not `NULL``, how many different of colours should be plotted.
#'
#' @param palette Colour pallette options passed to [leaflet::colorNumeric()].
#'
#' @param legend_pos Legend position. Accepts `c("topright", "bottomright", "bottomleft","topleft")`.
#'
#' @param legend_title Legend title.
#'
#' @param opacity Opacity of the edge of the geometry.
#'
#' @param fill_opacity Internal opacity of the geometry.
#'
#' @param width Width of the map
#'
#' @param height Height of the map
#'
#' @param padding Padding of the map
#'
#' @param type If data is data frame, what type of spatial object it is.
#'
#' @param latitude,longitude Columns for latitude and longitude
#'
#' @param projection The projections system
#'
#' @param id If the data type is not point, an id column with the groupings.
#'
#' @param ... Other arguments passed to [leaflet::addControl()]
#'
#' @author Stuart K. Grange, Hao Wu
#'
#' @return Invisible, a leaflet map.
#'
#' @import leaflet
#'
#' @export
plot_leaflet <- function(data, popup = NULL, force = TRUE, colour = "#03F",
                         col_group = NULL, n = 7, palette = "viridis",
                         legend_pos = "topright", legend_title = NULL,
                         opacity = 0.5, fill_opacity = 0.2,
                         width = NULL, height = NULL, padding = 0,
                         type, latitude = "latitude",
                         longitude = "longitude",
                         projection = projection_wgs84(), id = NA,
                         ...) {

  if (any(class(data) == "data.frame")) {

    sp <- sp_from_data_frame(data, type = type,
                             latitude = latitude, longitude = longitude,
                             projection = projection, keep = TRUE, id = id)

  } else sp <- data

  # Find geom type
  sp_class <- sp_class(sp)

  # Sort out popups
  # Use name variable even if not declared
  if (grepl("data", sp_class, ignore.case = TRUE)) {

    # Use a default
    if (is.null(popup) && "name" %in% names(sp@data)) popup <- "name"

    if (!is.null(popup)) {

      popup <- create_popup_string(sp@data, popup)

    }

  }

  #
  # Projection force
  if (force) sp <- sp_transform(sp, warn = FALSE)

  # Create map
  map <- leaflet(sp, width, height, padding) %>%
    addTiles(
      group = "OpenStreetMap",
      urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
    ) %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner lite") %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "landscape", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Landscape"
    ) %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "transport-dark", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Transport dark"
    ) %>%
    addTiles(
      urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
      attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
      options = tileOptions(variant = "outdoors", apikey = "25ef91f0102248f4a181998ec2b7a1ad"),
      group = "Outdoors"
    ) %>%
    addProviderTiles("Esri.WorldImagery", group = "Images") %>%
    addLayersControl(
      baseGroups = c(
        "OpenStreetMap", "Toner", "Toner lite", "Landscape", "Transport dark",
        "Outdoors", "Images"
      )
    )

  # sort out colours
  # colour is a column name in the data
  if (colour %in% names(sp)) {

    # keep a record of the name of the column for colour mapping
    colour_col <- colour

    # colour is numeric
    # we need regular pal for plotting and a reverse pal for legend
    if (is.numeric(sp[[colour]])) {
      if (is.null(col_group)) {

        pal <- colorNumeric(palette,
                            domain = sp[[colour]])

        pal_rev <- colorNumeric(palette,
                            domain = sp[[colour]],
                            reverse = TRUE)

      } else if (col_group == "bin") {


        pal <- colorBin(palette,
                        domain = sp[[colour]],
                        bins = n)

        pal_rev <- colorBin(palette,
                            domain = sp[[colour]],
                            bins = n,
                            reverse = TRUE)

      } else if (col_group == "quantile") {

        pal <- colorQuantile(palette,
                             domain = sp[[colour]],
                             n = n)

        pal_rev <- colorQuantile(palette,
                                 domain = sp[[colour]],
                                 n = n,
                                 reverse = TRUE)

      }

      colour <- pal(sp[[colour]])

    } else {

      # colour is character, change to factor
      if (is.character(sp[[colour]])) {

        sp[[colour]] <- factor(sp[[colour]])

      }

      # colour is factor
      if (is.factor(sp[[colour]])) {

        pal <- colorFactor(palette, domain = sp[[colour]])

        pal_rev <- colorFactor(palette, domain = sp[[colour]], reverse = TRUE)

        colour <- pal(sp[[colour]])
      }

    }

  }

  # Add layers
  if (grepl("points", sp_class, ignore.case = TRUE)) {

    map <- map %>%
      addCircleMarkers(
        popup = popup,
        color = colour,
        opacity = opacity,
        fillOpacity = fill_opacity,
        ...
      )

  } else if (grepl("lines", sp_class, ignore.case = TRUE)) {

    map <- map %>%
      addPolylines(
        popup = popup,
        color = colour,
        opacity = opacity,
        fillOpacity = fill_opacity,
        ...
      )

  } else if (grepl("polygons", sp_class, ignore.case = TRUE)) {

    map <- map %>%
      addPolygons(
        popup = popup,
        color = colour,
        opacity = opacity,
        fillOpacity = fill_opacity,
        ...
      )

  } else if (sp_class == "RasterLayer") {

    # Just a first step, raster and layer control has some issues
    map <- leaflet() %>%
      addTiles(
        group = "OpenStreetMap",
        urlTemplate = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
      ) %>%
      addRasterImage(sp, colors = "viridis", opacity = opacity)

  }

  # Add legend
  if (length(colour) > 1) {

    map <- map %>%
      addLegend(
        legend_pos,
        pal = pal_rev,
        values = sp[[colour_col]],
        title = legend_title,
        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
      )
  }

  return(map)

}
