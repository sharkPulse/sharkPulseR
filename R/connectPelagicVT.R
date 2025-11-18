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
  	dbname = "sharkpulse"
    dbhost <- "sp2.cs.vt.edu"
  	dbport <- 5432
  	drv <- dbDriver("PostgreSQL") 
  	con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname,  user=dbuser, password=dbpass
  	) 
}

#' Create Connection to the Mediterranean Monitoring Database.
#' This connection grant permission to select on the sharkpulse table. 
#' @param dbuser role name for database
#' @param dbpass database password for role `dbuser`
#' @param db database password for role `dbuser`
#' @examples
#' con = connectMed()
#' dat
#' @export
connectMed = function(dbuser, dbpass, db = "med_monitoring"){
  require(RPostgreSQL) 	
  	dbname = db
    dbhost <- "sp2.cs.vt.edu"
  	dbport <- 5432
  	drv <- dbDriver("PostgreSQL") 
  	con <- dbConnect(drv, host=dbhost, port=dbport, dbname=dbname,  user=dbuser, password=dbpass
  	) 
}