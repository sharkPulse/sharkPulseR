#' Remove points inland
#' 
#' Takes a dataset of observation from Flickr and associated distance from coast. Identifies points inland and remove those > buffer km from coast
#' @param dat dataset of points
#' @param buffer distance to use as a bufer to retain points
#' @export
removeInlands = function(dat, buffer = 10){
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(dplyr)
  library(units)
  # Turn off s2 for operations needing planar geometry
  sf::sf_use_s2(FALSE)
  
  # Load world data and transform to WGS84, simplify polygons
  world <- ne_countries(scale = 50, returnclass = "sf") %>%
    st_transform(3395) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 10000)  # simplification in meters

  # Create an sf object from dat using longitude and latitude and transform it
  pts <- dat %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(3395)  # transform points to the same CRS as the world

  # Buffer around coastline (10 km converted to meters)
  buffer_dist <- set_units(buffer, "km")
  buffered_coastlines <- st_buffer(st_geometry(world), dist = as.numeric(buffer_dist))

  # Points on land considering buffer
#   pts_on_land <- st_intersects(pts, buffered_coastlines, sparse = FALSE)
#   inland_pts <- dat[apply(pts_on_land, 1, any), ]  # select points not within the buffered zone

  # Determine which points are within the buffered coastlines
  pts_within_buffer <- st_intersects(pts, buffered_coastlines, sparse = FALSE)

  # Create 'inland' column: TRUE if not within buffered zone, FALSE otherwise
  dat$inland <- apply(pts_within_buffer, 1, any)

  return(dat)
}


