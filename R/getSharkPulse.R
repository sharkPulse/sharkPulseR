#' Get the entire sharkPulse table via DB or API
#'
#' @param dbuser sCPUEdb user with privileges to access the sharkpulse table
#' @param dbpass sCPUEdb password for user with privileges to access the sharkpulse table
#' @param external Boolean indicating whether the user is external (True) or internal (False, default)
#' @export
getSharkPulse = function(dbuser, dbpass, external = FALSE) {
  require(lubridate)
  require(httr)
  require(jsonlite)
  
  # Internal: Access via database directly
  if (!external) {
    con <- connectPelagic(dbuser, dbpass)
    
    # Sharkpulse
    query1 <- "SELECT common_name, species_name, latitude, longitude, date, location, img_name, source, source_type 
               FROM sharkpulse 
               WHERE latitude IS NOT NULL AND validated='t' AND species_name IS NOT NULL;"
    sharkpulse <- dbGetQuery(con, query1)
    sharkpulse$table <- 'sharkpulse'
    
    # Flickr
    query2 <- "SELECT common_name_1 AS common_name, species_name_1 AS species_name, latitude, longitude, date, img_name, '' AS location , 'Flickr' AS source 
               FROM flickr 
               WHERE validated='t' AND latitude IS NOT NULL AND species_name_1 IS NOT NULL;"
    flickr <- dbGetQuery(con, query2)
    flickr$source_type <- "Flickr"
    flickr$table <- "flickr"
    
    # iNaturalist
    query3 <- "SELECT common_name, scientific_name AS species_name, datetime AS date, latitude, longitude, place_guess AS location, img_name, 'iNaturalist' AS source 
               FROM inat 
               WHERE latitude IS NOT NULL AND scientific_name IS NOT NULL AND quality_grade='research';"
    inat <- dbGetQuery(con, query3)
    inat$date <- as.Date(ymd_hms(inat$date))
    inat$source_type <- "iNaturalist"
    inat$table <- "inat"
    
    # Instagram
    query4 <- "SELECT common_name, species_name, latitude, longitude, location, img_name, date, 'Instagram' AS source 
               FROM instagram 
               WHERE repost!='t' AND aquarium!='t' AND validated='t' AND shark='shark' AND species_name IS NOT NULL;"
    instagram <- dbGetQuery(con, query4)
    instagram$source_type <- "Instagram"
    instagram$table <- 'instagram'
    
    # New Flickr -- query removes duplicates of Flickr records
    query5 <- "SELECT common_name_cs AS common_name, species_name_cs AS species_name, datetaken AS date, latitude, longitude, '' AS location, img_name, 'Flickr' AS source 
               FROM flickr_new fn 
               WHERE validated='t' AND latitude IS NOT NULL AND species_name_cs != '' 
               AND NOT EXISTS (SELECT 1 FROM flickr f WHERE fn.url_m = f.img_name);"
    flickr_new <- dbGetQuery(con, query5)
    flickr_new$date <- as.Date(ymd_hms(flickr_new$date))
    flickr_new$source_type <- "Flickr"
    flickr_new$table <- "flickr_new"
    
    # Combine data from different sources into one dataframe
    dat <- rbind(sharkpulse, flickr, flickr_new, inat, instagram)
    colnames(dat) <- c("common_name", "species_name", "latitude", "longitude", "date", "location", "img_name", "source", "source_type", "table")
    
    dbDisconnect(con)
    return(dat)
    
  } else {
    # External: Access via API
    url <- paste0("http://sp2.cs.vt.edu:8086/get_combined_data?username=", dbuser, "&password=", dbpass)
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed", simplifyVector = TRUE)
      dat <- as.data.frame(data)
      return(dat)
    } else {
      stop("Failed to retrieve data: ", status_code(response), " - ", content(response, "text"))
    }
  }
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
