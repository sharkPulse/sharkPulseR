#' Remove points inland
#' 
#' Takes a dataset of observation from Flickr and associated distance from coast. Identifies points inland and remove those > buffer km from coast
#' @param dat dataset of points
#' @param buffer distance to use as a bufer to retain points
#' @export
removeInlands = function(dat, buffer = 10){
  library(rnaturalearth)
  library(rnaturalearthdata)
  require(sf)
  sf::sf_use_s2(FALSE)
  world <- ne_countries(scale = 50, returnclass = "sf")
  world <- st_transform(world, 4326)

  pts <- dat %>%
    sf::st_as_sf(coords = c("longitude", "latitude")) 
  st_crs(pts) <- 4326
  pts_on_land <- st_intersection(pts, world)
  # leave points within buffer km from coast
  pts_on_land = pts_on_land[pts_on_land$distance>buffer,]

  ph.oce = dat[!dat$url_m %in% pts_on_land$url_m,] # photos in ocean
  ph.oce

}
