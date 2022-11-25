#' Remove records close to aquaria
#'
#' The function will filter records by removing those close to aquaria
#' @param aquaria location data of aquaria
#' @param records location data of records
#' @export 
removeAqua = function(aquaria = aqua, records = dat){

	require(sharkPulseR)
	require(sCPUEdb)
	
	#world<-sf::st_read("../maps/worldLand") # shape file for all countries in the world
	#	aqua = sf::st_as_sf(aqua, coords = c("longitude","latitude"), crs = sf::st_crs(world)) # this comes from  the database
	mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
	# this is for converting the coordinate system to metric from degrees
	aqua.moll = sf::st_as_sf(aqua, coords = c("longitude","latitude"), crs = CRS(mollweide)) # this comes from  the database
	#aqua.moll <- st_transform(aqua,CRS(mollweide))
	aqua_buffer = sf::st_buffer(aqua.moll, 1000) # create a 1 km buffer around the points
	#pictures.sp = sf::st_as_sf(records, coords = c("longitude","latitude"), crs = sf::st_crs(world))
	pic.moll = sf::st_as_sf(records, coords = c("longitude","latitude"), crs = CRS(mollweide))
	#pic.moll <- st_transform(pictures.sp,CRS(mollweide))
	#test = sf::st_contains(pic.moll, aqua_buffer, sparse = FALSE)
	test = st_intersects(pic.moll, aqua_buffer, sparse = FALSE)
# it returns a matrix of logical values. these are whether any x (pictures) is within any y polygon (aquarium buffers)
# remove those that are true
	inaqua = rowSums(test) # this finds the pictures in aquaria (which rows have at least one TRUE case)
	records = records[inaqua==0,]
	records # it returns the filtered photo dataset
}