#' Write a google sheet and render images with the IMAGE() formula
#'
#' Create a google sheet from a data frame
#' @param df data frame to supply
#' @param sheet_name name of the google sheet 
#' @export

writeGS <- function(df, sheet_name) {
  library(googlesheets4)
  googlesheets4::gs4_auth()

  df$img_name = as.character(df$img_name)
  # Append '.jpg' if img_name does not contain a period '.'
  df$img_name <- ifelse(grepl("\\.", df$img_name), df$img_name, paste0(df$img_name, ".jpg"))

  # Create the 'img_render' column with the conditional check
  df$torender <- ifelse(grepl("http", df$img_name),
                          paste0(df$img_name),
                          paste0('https://sp2.cs.vt.edu/shiny/pulseMonitor/www/', df$img_name))
  # Reorder the columns to make 'img_render' the first column
  df <- df[, c("torender", setdiff(names(df), "torender"))]
  
  # Create the Google Sheet without the img_render column
  sheet <- gs4_create(sheet_name, sheets = list("Data" = df))
  
  # Get the spreadsheet ID
  sheet_id <- as_sheets_id(sheet)
  
  # Construct the sheet URL
  sheet_url <- paste0("https://docs.google.com/spreadsheets/d/", sheet_id)

  # Print the URL for reference
  print(sheet_url)
  
  return(list(dataframe = df, sheet_url = sheet_url))
}

# setwd("/var/www/html/sharkPulse/sp-database")
# dat = read.csv("data/checkInlands.csv")
# torender = writeGS(dat, "sp_inlands")
# torender[2] # link