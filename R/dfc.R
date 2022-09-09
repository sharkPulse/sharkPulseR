#' Return Distance from Coast
#'
#' Calculate distance from coast of a set of points
#' @param dat dataset of points. this should not contain missing coordinates
#' @param sample whether a sample should be taken for calculating distance - this process takes approximately 2 min for 1000 points
#' @param nsamples number of samples to take
#' @export
dfc = function(dat, sample = TRUE, nsamples = 50){
	# developed from https://stackoverflow.com/questions/27697504/ocean-latitude-longitude-point-distance-from-shore
	library(rgdal)   # for readOGR(...); loads package sp as well
	library(rgeos)   # for gDistance(...)
	
# WGS84 long/lat
	wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# ESRI:54009 world mollweide projection, units = meters
# see http://www.spatialreference.org/ref/esri/54009/
	mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

	dat = subset(dat, !is.na(latitude)) # there should not be na coordinates
	sp.points <- SpatialPoints(dat[,c("longitude","latitude")], proj4string=CRS(wgs.84))

	coast  <- readOGR(dsn="../data/ne_10m_coastline/",layer="ne_10m_coastline",p4s=wgs.84) # shapefiles are in data and dowlodaded here http://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-coastline/
	coast.moll <- spTransform(coast,CRS(mollweide))
	point.moll <- spTransform(sp.points,CRS(mollweide))

if (sample) test   <- sample(1:length(sp.points),500) else test =  1:length(sp.points)
	result <- sapply(test,function(i)gDistance(point.moll[i],coast.moll))/1000 # distance in km
	cbind(dat[test,],distance = result)
}
