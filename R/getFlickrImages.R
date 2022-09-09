#' Get Flickr Images 
#'
#' The function will connect to Flickr and extract image links with a given tag ("shark") and then filterm them excluding points on land.
#' @param text flickr tag to use to extract images
#' @param geobox character string for a geographic box "west, south, east, north"
#' @return it writes a file \code{output.csv} in the \code{py} folder. 
#' @export  
getFlickrImages = function(text, geoBox = "-180,-90,180,90"){

 	require(flickRgeotag)
 	api_key = "7bf4ce840f517255cce4295b2c753b63"
	photos <- flickr.photos.search(api_key = api_key, geoBox = geoBox, text = text, .allpages = TRUE)
    if(nrow(photos) == 0 ) stop 
  datt = photos[,c("datetaken","latitude","longitude","url_m","id","owner")]
  datt$flickr_url = paste("http://flickr.com/photos/",datt$owner,"/",datt$id, sep = "")
  datt$imageRender = paste("=IMAGE(\"",datt$url_m,"\")", sep = "")
  datt

}