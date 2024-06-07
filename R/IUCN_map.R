#' Plot SP data over IUCN distribution map
#'
#' 
#' @param species scientific name of the focal species
#' @param records usually sharkpulse data but can be any occurrence record dataset
#' @param shapes shaape files for plotting the expected distribution range
#' @export

IUCN_map <- function(species, records = dat, shapes = shark_sh ){
  

  # load libraries

  require(mapdata)
  require(tidyverse)
  require(magrittr)
  require(rmapshaper)
  # load IUCN shapefiles
  #shark_sh <- shark_sh
  

  # species <-  "Hypanus"
  # filter shapefile of single species
  spec_sl <- shark_sh %>% filter(binomial == species) # extract the specific polygon
  
  if (nrow(spec_sl) != 0){
  #   simplify the shapefile to plot it easily
  spec_sl <- ms_simplify(spec_sl, keep_shapes = TRUE, explode = FALSE)
  }
 
  
  world <- maps::map("world", xlim = c(-180, 180), ylim = c(-90, 90), fill = TRUE, lforce = "e", plot = FALSE)

  
  # dat <- dat
  # sharks <- sharks
  SPdb <- records
  SPdb %<>% filter(!is.na(longitude)) 

  
  # filter the required species
  spec <- SPdb %>% filter(species_name == species)
  
  if (nrow(spec) != 0){
  # jitter the coordinates
  spec %<>% mutate(longitude = jitter(longitude, factor = 0.01),
                   latitude = jitter(latitude, factor = 0.01)) 
  }
  
  #IUCN <- sharks %>% filter(scientific_name == species)
  
  if (nrow(spec_sl) == 0) {
    ggplot() +
      
      geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), col = "gray25", bg = "gray60") +
     
      geom_point(data = spec, aes(x = longitude, y = latitude),
                 colour = "white", shape = 21, fill = "firebrick1", size = 3) +
      labs(x = "Longitude", y = "Latitude", title = species,
           subtitle = paste(unique(spec$main_common_name), unique(spec$category), sep = " - ")) +
      #scale_fill_manual(values = c("dodgerblue4", "deepskyblue")) +
      coord_sf() +
      theme_void() +
      theme(legend.position = c(0.15, 0.25),
            plot.title = element_text(hjust = 0.1),
            plot.subtitle =  element_text(hjust = 0.1))
  } else if (nrow(spec_sl) > 2) {
    # plot the results
    ggplot() +
      geom_sf(data = spec_sl, aes(fill = legend), alpha = 0.8) +
      geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), col = "gray25", bg = "gray60") +
      
      geom_point(data = spec, aes(x = longitude, y = latitude),
                 colour = "white", shape = 21, fill = "firebrick1", size = 3) +
      labs(x = "Longitude", y = "Latitude", fill = "IUCN\nSpatial\nDistribution", title = species,
           subtitle = paste(unique(spec$main_common_name), unique(spec$category), sep = " - ")) +
      scale_fill_manual(values = c("dodgerblue4",  "cyan", "deepskyblue", "lightblue1")) +
      theme_void() +
      theme(legend.position = c(0.15, 0.25),
            plot.title = element_text(hjust = 0.1),
            plot.subtitle =  element_text(hjust = 0.1))
  } else {
    
    ggplot() +
      geom_sf(data = spec_sl, aes(fill = legend), alpha = 0.8) +
      geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), col = "gray25", bg = "gray60") +
     
      geom_point(data = spec, aes(x = longitude, y = latitude),
                 colour = "white", shape = 21, fill = "firebrick1", size = 3) +
      labs(x = "Longitude", y = "Latitude", fill = "IUCN\nSpatial\nDistribution", title = species,
           subtitle = paste(unique(spec$main_common_name), unique(spec$category), sep = " - ")) +
      scale_fill_manual(values = c("dodgerblue4",  "deepskyblue")) +
      theme_void() +
      theme(legend.position = c(0.15, 0.25),
            plot.title = element_text(hjust = 0.1),
            plot.subtitle =  element_text(hjust = 0.1))
    
    
  }
  
  
}




IUCN_map2 <- function(species, records = dat, shapes = shark_sh) {

library(ggplot2)
library(sf)
require(mapdata)
require(tidyverse)
require(magrittr)
require(rmapshaper)

  # Load IUCN shapefiles
  spec_sl <- shapes %>% filter(binomial == species) # extract the specific polygon
  
  if (nrow(spec_sl) != 0) {
    # Simplify the shapefile to plot it easily
    spec_sl <- ms_simplify(spec_sl, keep_shapes = TRUE, explode = FALSE)
  }
  
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





