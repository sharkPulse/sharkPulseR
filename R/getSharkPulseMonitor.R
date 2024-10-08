
#' Retrieve pulse monitor
#'
#' @param dbuser sp user with privileges to access the sharkpulse table
#' @param dbpass sp password for user with privileges to access the sharkpulse table
#' @export
getSharkPulseMonitor = function(dbuser, dbpass){
	#require(sCPUEdb)
	require(lubridate)
	require(dplyr)
	con = connectPelagic(dbuser, dbpass)
#	dat = dbSendQuery(con, statement = paste("select * from sharkpulse;",sep=""))
#	dat = fetch(dat, n = -1) # gets the index of abundance
#	dbDisconnect(con)
#	dat

	# Sharkpulse
	query1 <- "SELECT common_name, species_name, latitude, longitude, date, location, img_name, source FROM sharkpulse WHERE latitude IS NOT NULL AND validated='t' AND species_name IS NOT NULL;"
	sharkpulse <- dbGetQuery(con, query1)

	# Flickr
	query2 <- "SELECT common_name_1 AS common_name, species_name_1 AS species_name, latitude, longitude, date, img_name, '' AS location , 'Flickr' AS source FROM flickr WHERE validated='t' AND latitude IS NOT NULL AND species_name_1 IS NOT NULL;"
	flickr <- dbGetQuery(con, query2)

	# iNaturalist
	query3 <- "SELECT common_name, scientific_name AS species_name, datetime AS date, latitude, longitude, place_guess AS location, img_name, 'iNaturalist' AS source FROM inat WHERE latitude IS NOT NULL AND scientific_name IS NOT NULL;"
	inat <- dbGetQuery(con, query3)
	inat$date <- as.Date(ymd_hms(inat$date))

	# Instagram
	query4 <- "SELECT common_name, species_name, latitude, longitude, location, img_name, date, 'Instagram' AS source FROM instagram WHERE repost!='t' AND aquarium!='t' AND validated='t' AND shark='shark' AND species_name IS NOT NULL;"
	instagram <- dbGetQuery(con, query4)

	# New Flickr -- query removes duplicates of Flickr records
	query5 <- "select common_name_cs as common_name, species_name_cs as species_name, datetaken as date, latitude, longitude, '' as location, img_name, 'Flickr' as source from flickr_new fn where validated='t' and latitude is not null and species_name_cs!='' and not exists (select 1 from flickr f where fn.url_m = f.img_name)"
	flickr_new <- dbGetQuery(con, query5)
	flickr_new$date = as.Date(ymd_hms(flickr_new$date))
	
	# Retrieve mapping from taxonomy3
	taxonomy_query <- "SELECT valid AS species_name, main_common_name FROM taxonomy3;"
	taxonomy_mapping <- dbGetQuery(con, taxonomy_query)

	# Combine data from different sources into one dataframe
	shark <- rbind(sharkpulse, flickr, flickr_new, inat, instagram)
	# Ensure column names are consistent across dataframes
    colnames(shark) <- c("common_name", "species_name", "latitude", "longitude", "date", "location", "img_name", "source")

    shark <- shark %>%
        semi_join(taxonomy_mapping, by = c("species_name" = "species_name"))
    # Step 4: Close the database connection
    # dbDisconnect(con)

    shark$img_name = ifelse((grepl('\\.', shark$img_name) & !grepl('http', shark$img_name)), paste0("www/",shark$img_name), shark$img_name)
    shark$img_name = ifelse((!is.na(shark$img_name) & !grepl('http', shark$img_name) & !grepl('\\.', shark$img_name)), 
            paste0("www/",shark$img_name, ".jpg"), shark$img_name)
    shark$species_name = factor(shark$species_name)
    shark$common_name = factor(shark$common_name)

    shark <- shark %>%
        mutate(full_img_path = paste0("www/", img_name)) %>%
        filter(sapply(full_img_path, file.exists)) %>%
        select(-full_img_path)  # Optionally, remove the full_img_path column if it's no longer needed

	dbDisconnect(con)
	shark 
}	