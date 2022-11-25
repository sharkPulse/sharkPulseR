#' Create Connection to the Pelagic Database on sharkpulse.cnre.vt.edu. 
#'
#' Create Connection to the Pelagic Database.
#' This connection grant permission to select on the sharkpulse table. 
#' @param dbuser role name for database
#' @param dbpass database password for role `dbuser`
#' @examples
#' con = connectPelagic()
#' dat = selectData(con, "select * from sharkpulse limit1")
#' dat
#' @export
connectPelagic = function(dbuser, dbpass){
  require(RPostgreSQL)
  require(RH2) 	
  	dbname = "pelagic"
  	#dbhost <- "sharkpulse.cnre.vt.edu"
    dbhost <- "sp2.cs.vt.edu"
  	dbport <- 5432
  	drv <- dbDriver("PostgreSQL") 
  	con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname,  user=dbuser, password=dbpass
  	) 
}