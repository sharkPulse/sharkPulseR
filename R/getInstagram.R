#' Get the entire sharkPulse table
#'
#' @param dbuser sCPUEdb user with privileges to access the sharkpulse table
#' @param dbpass sCPUEdb password for user with privileges to access the sharkpulse table
#' @export

getInstagram = function(dbuser, dbpass){
	con = connectPelagic(dbuser, dbpass)
	dat = dbSendQuery(con, statement = paste("select * from instagram;",sep=""))
	dat = fetch(dat, n = -1) # gets the index of abundance
	dbDisconnect(con)
	dat
}
