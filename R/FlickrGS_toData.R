require(DBI)
require(RPostgreSQL)
require(RH2)
require(dplyr)
source("connectPelagicB3.R")

con = connectPelagic()

data_mining = fetch(dbSendQuery(con, statement = paste("select * from data_mining;",sep="")), n=-1)
googlesheet = read.csv('global.csv')

googlesheet$date = ''
for (i in 1:nrow(googlesheet)) {
  googlesheet$date[i] = unlist(strsplit(googlesheet$datetaken[i], ' '))
}
googlesheet$time = ''
for (i in 1:nrow(googlesheet)) {
  googlesheet$time[i] = unlist(strsplit(googlesheet$datetaken[i], ' '))[2]
}
val = which(googlesheet$Shark.!='')
googlesheet = googlesheet[-c(val),]
non_url = which(is.na(googlesheet$url_m))
googlesheet = googlesheet[-c(non_url),]
googlesheet$latitude = as.numeric(format(round(googlesheet$latitude, 2), nsmall = 1))
googlesheet$longitude = as.numeric(format(round(googlesheet$longitude, 2), nsmall = 1))
data_mining$date = as.character(data_mining$date)

new_dat = googlesheet %>% anti_join(data_mining, by = c("longitude", "latitude", "date"))
new_dat$imageRender = paste("=IMAGE(\"",new_dat$url_m,"\")", sep = "")

write.csv(new_dat, '../../s/predictor/dredge1.csv', row.names = F)

# system(paste('python ../../s/predictor/identifier.py'))


