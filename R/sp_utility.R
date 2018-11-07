#' Function to find class of spatial object as a vector with a length of 1.
#'
#' @param sp Spatial object.
#'
#' @author Stuart K. Grange
#'
#' @export
sp_class <- function(sp) class(sp)[1]

#' Function to return a spatial object's projection system.
#'
#' @param sp Spatial object
#'
#' @author Stuart K. Grange
#'
#' @export
sp_projection <- function(sp) sp::proj4string(sp)
