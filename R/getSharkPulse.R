#' Get the entire sharkPulse table
#'
#' @param dbuser sCPUEdb user with privileges to access the sharkpulse table
#' @param dbpass sCPUEdb password for user with privileges to access the sharkpulse table
#' @export
getSharkPulse = function(dbuser, dbpass){
	#require(sCPUEdb)
	con = connectPelagic(dbuser, dbpass)
	dat = dbSendQuery(con, statement = paste("select * from sharkpulse;",sep=""))
	dat = fetch(dat, n = -1) # gets the index of abundance
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