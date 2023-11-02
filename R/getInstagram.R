#' Get the instagram table
#'
#' @param dbuser user with privileges to access the instagram table
#' @param dbpass password for user with privileges to access the instagram table
#' @export

getInstagram = function(dbuser, dbpass){
	con = connectPelagic(dbuser, dbpass)
	dat = dbSendQuery(con, statement = paste("select * from instagram;",sep=""))
	dat = fetch(dat, n = -1) # gets the index of abundance
	dbDisconnect(con)
	dat
}
