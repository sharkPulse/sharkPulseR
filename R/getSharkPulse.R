#' Get the entire sharkPulse table
#'
#' @param dbuser sCPUEdb user with privileges to access the sharkpulse table
#' @param dbpass sCPUEdb password for user with privileges to access the sharkpulse table
#' @export
getSharkPulse = function(dbuser, dbpass){
	#require(sCPUEdb)
	require(lubridate)
	con = connectPelagic(dbuser, dbpass)
#	dat = dbSendQuery(con, statement = paste("select * from sharkpulse;",sep=""))
#	dat = fetch(dat, n = -1) # gets the index of abundance
#	dbDisconnect(con)
#	dat

	# Sharkpulse
	query1 <- "SELECT common_name, species_name, latitude, longitude, date, location, img_name, source, source_type FROM sharkpulse WHERE latitude IS NOT NULL AND validated='t' AND species_name IS NOT NULL;"
	sharkpulse <- dbGetQuery(con, query1)
	sharkpulse$table = 'sharkpulse'

	# Flickr
	query2 <- "SELECT common_name_1 AS common_name, species_name_1 AS species_name, latitude, longitude, date, img_name, '' AS location , 'Flickr' AS source FROM flickr WHERE validated='t' AND latitude IS NOT NULL AND species_name_1 IS NOT NULL;"
	flickr <- dbGetQuery(con, query2)
	flickr$source_type = "Flickr"
	flickr$table = "flickr"

	# iNaturalist
	query3 <- "SELECT common_name, scientific_name AS species_name, datetime AS date, latitude, longitude, place_guess AS location, img_name, 'iNaturalist' AS source FROM inat WHERE latitude IS NOT NULL AND scientific_name IS NOT NULL AND quality_grade='research';"
	inat <- dbGetQuery(con, query3)
	inat$date <- as.Date(ymd_hms(inat$date))
	inat$source_type = "iNaturalist"
	inat$table = "inat"

	# Instagram
	query4 <- "SELECT common_name, species_name, latitude, longitude, location, img_name, date, 'Instagram' AS source FROM instagram WHERE repost!='t' AND aquarium!='t' AND validated='t' AND shark='shark' AND species_name IS NOT NULL;"
	instagram <- dbGetQuery(con, query4)
	instagram$source_type = "Instagram"
	instagram$table = "instagram"

	# New Flickr -- query removes duplicates of Flickr records
	query5 <- "select common_name_cs as common_name, species_name_cs as species_name, datetaken as date, latitude, longitude, '' as location, img_name, 'Flickr' as source from flickr_new fn where validated='t' and latitude is not null and species_name_cs!='' and not exists (select 1 from flickr f where fn.url_m = f.img_name)"
	flickr_new <- dbGetQuery(con, query5)
	flickr_new$date = as.Date(ymd_hms(flickr_new$date))
	flickr_new$source_type = "Flickr"
	flickr_new$table = "flickr_new"
	
	# Combine data from different sources into one dataframe
	dat <- rbind(sharkpulse, flickr, flickr_new, inat, instagram)
	# Ensure column names are consistent across dataframes
	colnames(dat) <- c("common_name", "species_name", "latitude", "longitude", "date", "location", "img_name", "source","source_type", "table")
	dbDisconnect(con)
	dat 

}	


#' Substitute the entire sharkPulse table
#'
#' @param dbuser sCPUEdb user with privileges to access the sharkpulse table
#' @param dbpass sCPUEdb password for user with privileges to access the sharkpulse table
#' @export
subSharkPulse = function(dbuser, dbpass, csvfile){
	#require(sCPUEdb)
	con = connectPelagic(dbuser, dbpass)
	dat = read.csv(csvfile)
	dataHeaders = dbSendQuery(con, statement = "select * from sharkpulse limit 1;")
	dataHeaders <- fetch(dataHeaders)
	
	dat = dat[, names(dataHeaders)]
	
	dbWriteTable(con, "sharkpulse", value=dat, overwrite = TRUE, row.names=FALSE)
	dat
}	
