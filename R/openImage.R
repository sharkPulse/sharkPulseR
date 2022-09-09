#' Open image from the sarkPulse archive
#'
#' this function require an image name as indicated in the sharkPulse table. Use selectData to get the image names of desired data.
#' @param img_name name of teh image to open
#' @export 
openImage = function (img_name, baselineUser = "francesco") 
{

    system(paste("scp -P 51001 ", baselineUser, "@sharkpulse.cnre.vt.edu:/srv/shiny-server/pulseMonitor/www/", img_name, ".* .", sep = ""))

    system(paste("open ", img_name, ".*", sep = ""))

    system(paste("rm ", img_name, ".*", sep = ""))

}
