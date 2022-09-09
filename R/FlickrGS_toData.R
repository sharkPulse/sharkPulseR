#' Flickr Dredge to csv
#'
#' @param con connection object
#' @export
Flickr_toData = function(con){
#require(DBI)
#require(RPostgreSQL)
#require(RH2)
require(dplyr)
require(googlesheets4)
#source("connectPelagicVT.R")

dm = fetch(dbSendQuery(con, statement = paste("select * from data_mining;",sep="")), n=-1)
#gs = read.csv('../data/flickrDregdeCommonNames.csv')
gs = read_sheet("https://docs.google.com/spreadsheets/d/1FuP2HHMHwoBGka3h49QtLNIg4eYj2Zja4PcJsoQF6wU/edit#gid=1512004190") # this googlesheet has been created on sept 13, 2021 12:22 am
names(gs)[5] = "flickr_id"

gs = as.data.frame(gs)
gs$datetaken = as.character(gs$datetaken)

gs$date = '' # creating a dater veriable to have time and date separate
for (i in 1:nrow(gs)) {
  gs$date[i] = unlist(strsplit(gs$datetaken[i], ' '))
}

gs$time = '' # creating the time variable
for (i in 1:nrow(gs)) {
  gs$time[i] = unlist(strsplit(gs$datetaken[i], ' '))[2]
}

# let's keep these for now
val = which(gs[,"Shark?"]!='') # excludes rows that are already validated
gs = gs[-c(val),]

gs2 = gs # just i case something goes wrong

non_url = which(is.na(gs$url_m)) # some rows did not have url so exclude those
if (length(non_url)!=0) gs = gs[-c(non_url),]

gs$latitude2 = as.numeric(format(round(gs$latitude, 2), nsmall = 1)) # this is for merging but I want to retain the high definition coords
gs$longitude2 = as.numeric(format(round(gs$longitude, 2), nsmall = 1)) # 
dm$latitude2 = dm$latitude
dm$longitude2 = dm$longitude

dm$date = as.character(dm$date)

nd = merge(dm, gs, by = c("longitude2", "latitude2", "date", "time"), all = TRUE) # nd stands for new data - I think it is safe if we leave all data. We may need to do some cleaning later on
# for the next dredge 
# nrow(gs)+nrow(dm)-nrow(nd) # there are 658 common records
nd = unique(nd) # we will work on sanitizing this data table then

nd = gs %>% anti_join(dm, by = c("longitude", "latitude", "date", "time"))
nd$imageRender = paste("=IMAGE(\"",nd$url_m,"\")", sep = "")

write.csv(new_dat, '../../s/predictor/dredge1.csv', row.names = F)

# system(paste('python ../../s/predictor/identifier.py'))
}

