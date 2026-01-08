
#' Load FAO Major Fishing Areas
#'
#' @return An sf object with FAO fishing areas
#' @export
load_fao_areas <- function() {
  # Path to the uploaded shapefile (ensure all associated files are in the same directory)
  #shapefile_path <- "extdata/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp"

  shp <- system.file(
    "extdata/FAO_AREAS_CWP_NOCOASTLINE/FAO_AREAS_CWP_NOCOASTLINE.shp",
    package = "sharkPulseR"
  )

  #fao_areas <- sf::st_read(shapefile_path)
  #fao_areas <- sf::st_make_valid(fao_areas)
  #return(fao_areas)

  fao <- sf::st_read(shp, quiet = TRUE)
  sf::st_make_valid(fao)

}

#' Get FAO area for records
#'
#' Function to get the FAO zone names in English for a set of points.
#' @param dat usually sharkpulse data with latitude and longitude. It can be any occurrence record dataset.
#' @param fao_areas shape file of FAO major fisheries areas
#' @export
getFaoNames <- function(dat, fao_areas) {
  
  latitudes <- dat$latitude
  longitudes <- dat$longitude

  # Filter FAO areas to include only those with F_LEVEL equal to "MAJOR"
  fao_areas_major <- fao_areas %>% filter(F_LEVEL == "MAJOR")
  
  # Create a data frame of points
  points_df <- data.frame(longitude = longitudes, latitude = latitudes)
  # Convert the data frame to an sf object
  points_sf <- st_as_sf(points_df, coords = c("longitude", "latitude"), crs = st_crs(fao_areas))
  
  # Perform a spatial join between points and FAO areas
  joined <- st_join(points_sf, fao_areas_major, join = st_intersects)
  
  # Extract the zone names in English
  zone_names_en <- joined$NAME_EN
  
  return(zone_names_en)
}


#' Check if a point is within the vector of FAO areas indicated for the species
#' 
#' @export
isinfao <- function(fao_code, species_name, areas) {
  species_fao_areas <- areas[[species_name]]
  return(fao_code %in% species_fao_areas)
}


#' Map species records on a FAO map
#' 
#' @param species species name
#' @records sharkPulse records
#' @export
FAO_map <- function(species, records = dat) {

library(ggplot2)
library(sf)
require(tidyverse)

  
  world <- maps::map("world", xlim = c(-180, 180), ylim = c(-90, 90), fill = TRUE, plot = FALSE)
  
  SPdb <- records %>% filter(!is.na(longitude))
  
  # Filter the required species
  spec <- SPdb %>% filter(species_name == species)
  
  if (nrow(spec) != 0) {
    # Jitter the coordinates
    spec <- spec %>% mutate(longitude = jitter(longitude, factor = 0.01),
                            latitude = jitter(latitude, factor = 0.01))
  }
  
ggplot() +
    geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), col = "gray25", fill = "gray60") +
    geom_point(data = spec, aes(x = longitude, y = latitude, fill = infao),
               colour = "white", shape = 21, size = 3) +
    scale_fill_manual(name = "In FAO Area", values = c("TRUE" = "firebrick1", "FALSE" = "dodgerblue4")) +
    labs(x = "Longitude", y = "Latitude", title = species,
         subtitle = paste(unique(spec$main_common_name), unique(spec$category), sep = " - ")) +
    coord_sf() +
    theme_void() +
    theme(legend.position = c(0.15, 0.25),
          plot.title = element_text(hjust = 0.1),
          plot.subtitle = element_text(hjust = 0.1))
  
}



