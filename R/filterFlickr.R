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
	pictures = read.csv("../py/'",query,"'.csv")
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
aqua = read.table("../data/aquaCoords.txt", header = T)

pictures = getFlickrImages(query, startUploadDate, endUploadDate, geoBox)

#source("csv2html.R") # transforms a csv table to an html page
csv2html(pictures,file = paste("mypageUnfiltered",query,"from",startUploadDate,"to",endUploadDate,"at",geoBox,".html", sep = ""))
# at this point I need to create another column with all maps and another one with a simple switch yes/no for real free living or dead sharks


# remove pictures from land with the function over
#library(sp)
#library(maptools)
#library(mapdata)


world<-maptools::readShapeSpatial("~/Google Drive/sharkPulse/worldLand") # shape file for all countries in the world

# Now setting the projection. I have taken a string appropriate for USA
projection = sp::CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

# Projections for points and polygons need to be the same
pictures.sp = sp::SpatialPoints(pictures[,c("lon","lat")], proj4string = projection)
sp::proj4string(world) = projection

# Now checking whether the points are in the polygons
inland<-sp::over(pictures.sp, world) 


# all points are in polygons. I need to find a way to project the coasts a little inland 

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

