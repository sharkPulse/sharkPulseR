#' Write info of a specific database table into a csv file 
#'
#' This is done to edit the file and include column descriptions. The resulting file will be named [table_name]Info.csv
#' @param con database connection
#' @param table_name nameof the specific table 
#' @export 
writeTableInfo = function(con = con, table_name = "instagram"){
  
  query <- paste0(
  "SELECT column_name, data_type, ordinal_position, ",
  "pgd.description AS column_description ",
  "FROM information_schema.columns AS cols ",
  "LEFT JOIN pg_catalog.pg_description AS pgd ON (pgd.objoid = (quote_ident(cols.table_name)::regclass)::oid ",
  "AND pgd.objsubid = cols.ordinal_position) ",
  "WHERE cols.table_name = '", table_name, "';"
)
  table_info <- dbGetQuery(con, query)
  write.csv(table_info, paste("tables/",table_name,"Info.csv", sep = ""), row.names = FALSE)
}