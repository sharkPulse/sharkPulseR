#' Open image from the sarkPulse archive
#'
#' this function require an image name as indicated in the sharkPulse table and the training table. Use selectData to get the image names of desired data.
#' To work, you need to be able to access the remote server without password
#' @param img_name name of teh image to open
#' @param srvUser username in the remote server
#' @export 
openImage = function (img_name, srvUser = "francesco") 
{

    system(paste("scp ", srvUser, "@sp2.cs.vt.edu:/srv/shiny-server/pulseMonitor/www/", img_name, ".* .", sep = ""))
    # the other bucket is here /home/spr/sd_images/shark
    system(paste("scp ", srvUser, "@sp2.cs.vt.edu:/home/spr/sd_images/shark/", img_name, ".* .", sep = ""))

    system(paste("open ", img_name, ".*", sep = ""))

    #system(paste("rm ", img_name, ".*", sep = ""))

}
