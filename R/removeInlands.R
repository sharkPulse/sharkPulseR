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
  dat$buffer <- buffer
  return(dat)
}


#' Create a global buffer
#' 
#' Takes a global polygon  apply a buffer of `buffer_km` and generate a unified buffered polygon. If `buffer_km` is positive it expand the continents, if `buffer_km` is negative, it shriks the continents. When shrinking the continents, the funtion prevents that internal corridors (gaps) are created because of shrinking national borders. 
#' 
#' @param buffer_km distance in km to use as a bufer, negative values shrink the continents and positive values expand them. 
#' @export
getBufferedWorld <- function(buffer_km) {
library(sf)
library(rnaturalearth)
library(dplyr)
  # Load world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- st_transform(world, crs = 3857)  # Transform to a planar CRS


# Combine and then dissolve all polygons
  world_combined <- st_combine(world)
  world_dissolved <- st_union(world_combined)

  
  # Convert buffer from kilometers to meters (as CRS is in meters)
  buffer_meters <- buffer_km * 1000

# this part is trick to ensure that national  poligons are united together. Apply a positive buffer first if needed to close gaps, then dissolve, then apply negative buffer

  if (buffer_km < 0) {
    world_temp_buffered <- st_buffer(world_dissolved, dist = 100)  # Slightly expand
    world_dissolved <- st_union(world_temp_buffered)  # Re-dissolve if needed
    buffered_world <- st_buffer(world_dissolved, dist = buffer_meters)
  } else {
    buffered_world <- st_buffer(world_dissolved, dist = buffer_meters)
  }


  
  return(buffered_world)
}


#' Remove points inland
#' 
#' Takes a dataset of locational points (it needs to have latitude and longitude columns), and create a new variable classifying these points in inlands==TRUE if they are falling on land, and FALSE if they are falling in the ocean
#' @param dat dataset of locational points
#' @param buffer_km distance in km to use as a bufer, negative values shrink the continents and positive values expand them
#' @export
removeInlands3 <- function(dat, buffer_km) {


    buffered_world <- getBufferedWorld(buffer_km)

    # Create a points sf object from the entire dataframe
    points_sf <- st_as_sf(dat, coords = c("longitude", "latitude"), crs = 4326)
    points_sf <- st_transform(points_sf, crs = 3857)

    # Check intersections for all points at once
    intersects <- st_intersects(points_sf, buffered_world, sparse = TRUE)

    # Determine 'inland' status for all points
    dat$inland <- lengths(intersects) > 0

    return(dat)
}

