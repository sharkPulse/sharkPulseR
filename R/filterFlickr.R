#' Get Flickr Images 
#'
#' The function will connect to Flickr and extract image links with a given tag ("shark") and then filterm them excluding points on land.
#' @param query flickr tag to use to extract images
#' @param startUploadDate date string yyyy-mm-dd
#' @param endUploadDate date string yyyy-mm-dd
#' @param geobox character string for a geographic box "west, south, east, north"
#' @return it writes a file \code{output.csv} in the \code{py} folder. 
#' @export  
getFlickrImages = function(query, startUploadDate, endUploadDate, geoBox){

 # west, south, east, east
	setwd("../py")
	system(paste("python flickrSearchShark.py '",query,"' ",startUploadDate," ",endUploadDate," '",geoBox,"' 0",sep=""))
	setwd("../s")
	pictures = read.csv(paste0("../py/output.csv"))
	names(pictures) = c("lat","lon","datetaken","dateupload","url")
	pictures

}

getFlickrImages2 = function(query, startUploadDate, endUploadDate, geoBox){

 # west, south, east, east
 	require(flickRgeotag)
	system(paste("python flickrSearchShark.py '",query,"' ",startUploadDate," ",endUploadDate," '",geoBox,"' 0",sep=""))
	setwd("../s")
	pictures = read.csv("../py/output.csv")
	names(pictures) = c("lat","lon","datetaken","dateupload","url")
	pictures

}


#' Filter flickr Results 
#'
#' The function will crawl the Flickr database extract the images with the given tag  ("shark") and then filterm them excluding points on land.
#' @param startUploadDate date string yyyy-mm-dd
#' @param endUploadDate date string yyyy-mm-dd
#' @param geobox character string for a geographic box "west, south, east, north"
#' @export  
flickrFilter = function(query = "shark", startUploadDate = "2015-01-02", endUploadDate =  "2015-12-02", geoBox = "-180, -90, 180, 90"){
# query: keyword to use to get images from flickr
# startDate: yyyy-mm-dd
# endDate: yyyy-mm-dd
# geobox: west, south, east, north
# from aquaria.R
require(flickRgeotag)
require(googlesheets4)
require(sf)

# remove points from within aquaria locations

aqua = read.table("../data/aquaCoords.txt", header = T)
aqua = na.omit(aqua)
aqua = st_as_sf(aqua, coords = c("lon","lat"), crs = st_crs(world))

# create a 1 km buffer around the points
aqua_buffer = st_buffer(aqua, 1)

#pictures = getFlickrImages(query, startUploadDate, endUploadDate, geoBox)
photos <- flickr.photos.search(bbox = geoBox, text="squalo", .allpages = T)
photos[,c("longitude","latitude")] = lapply(photos[,c("longitude","latitude")], as.numeric)
qflickr.plot(photos)
pictures.sp = sf::st_as_sf(photos, coords = c("longitude","latitude"), crs = st_crs(world))

test = st_contains(pictures.sp, aqua_buffer, sparse = FALSE)
# it returns a matrix of logical values. these are whether any x is within any y polygon
# remove those that are true




# to retain only coastal points I can also calculate distance between points and closest coast.
# https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/


# this is promising
devtools::install_github("mdsumner/distancetocoast")
library(distancetocoast)
library(raster)

dists = raster::extract(distance_to_coastline_lowres, photos[,c("longitude","latitude")])/1000 # to return kilometers
qflickr.plot(photos[which(dists<10),]) # plots only points closer than 10 km to the coast



# We need to continue from here






# remove pictures from land with the function over
#library(sp)
#library(maptools)
#library(mapdata)


world<-maptools::readShapeSpatial("../maps/worldLand") # shape file for all countries in the world

world<-rgdal::readOGR("../maps/worldLand")
# Now setting the projection. I have taken a string appropriate for USA
projection = sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# Projections for points and polygons need to be the same
pictures.sp = sp::SpatialPoints(lapply(photos[,c("longitude","latitude")], as.numeric), proj4string = projection)
sp::proj4string(world) = projection
# Now checking whether the points are in the polygons
inland<-sp::over(pictures.sp, world) 



world <- sf::read_sf("../maps/worldLand")
# reduce world by some units  - usually degrees 
#https://gis.stackexchange.com/questions/392505/can-i-use-r-to-do-a-buffer-inside-polygons-shrink-polygons-negative-buffer
hrinkIfPossible <- function(sf, size) {
  # compute inward buffer
  sg <- st_buffer(st_geometry(sf), -size)
  
  # update geometry only if polygon is not degenerate
  st_geometry(sf)[!st_is_empty(sg)] = sg[!st_is_empty(sg)]
   
  # return updated dataset
  return(sf)
}

shp_buffered <- shrinkIfPossible(world, 10)

pictures.sp = sf::st_as_sf(photos, coords = c("longitude","latitude"), crs = st_crs(world))

inland = st_contains(pictures.sp, world)

# all points are in polygons. I need to find a way to project the coasts a little inland 
# reduce size shapefile xx km inland

pdf(paste("../maps/unfiltered",query,"FROM",startUploadDate,"TO",endUploadDate,"global.pdf",sep=""))
maps::map("worldHires",fill=TRUE)
points(lat~lon, pictures, col = "red", pch=16)
dev.off()

ocean = pictures[is.na(inland$OBJECTID),]

pdf(paste("../maps/filtered",query,"FROM",startUploadDate,"TO",endUploadDate,"global.pdf",sep=""))
maps::map("worldHires",fill=TRUE)
points(lat~lon, ocean, col = "green", pch=16)
dev.off()

write.csv(ocean,file = paste("../data/",query,"FROM",startUploadDate,"TO",endUploadDate,"global.csv",sep=""), row.names = F)


}

