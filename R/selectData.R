#' Select Data from Database
#'
#' @param con connection to database
#' @param statement postgreSQL statement
#' @export	


selectData = function(con, statement){
	dat = fetch(dbSendQuery(con, statement = statement), n = -1)
	dat
}	
	