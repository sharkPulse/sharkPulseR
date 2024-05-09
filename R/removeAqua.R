#' Remove records close to aquaria
#'
#' The function will filter records by removing those close to aquaria
#' @param aquaria location data of aquaria
#' @param records location data of records
#' @export 
removeAqua = function(aquaria = aqua, records = dat){

	require(sharkPulseR)
	#require(sCPUEdb)
	
	#world<-sf::st_read("../maps/worldLand") # shape file for all countries in the world
	#	aqua = sf::st_as_sf(aqua, coords = c("longitude","latitude"), crs = sf::st_crs(world)) # this comes from  the database
	mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
	# this is for converting the coordinate system to metric from degrees
	aqua.moll = sf::st_as_sf(aqua, coords = c("longitude","latitude"), crs = CRS(mollweide)) # this comes from  the database
	#aqua.moll <- st_transform(aqua,CRS(mollweide))
	aqua_buffer = sf::st_buffer(aqua.moll, 1000) # create a 1 km buffer around the points
	#pictures.sp = sf::st_as_sf(records, coords = c("longitude","latitude"), crs = sf::st_crs(world))
	pic.moll = sf::st_as_sf(records, coords = c("longitude","latitude"), crs = CRS(mollweide))
	#pic.moll <- st_transform(pictures.sp,CRS(mollweide))
	#test = sf::st_contains(pic.moll, aqua_buffer, sparse = FALSE)
	test = st_intersects(pic.moll, aqua_buffer, sparse = FALSE)
# it returns a matrix of logical values. these are whether any x (pictures) is within any y polygon (aquarium buffers)
# remove those that are true
	inaqua = rowSums(test) # this finds the pictures in aquaria (which rows have at least one TRUE case)
	records = records[inaqua==0,]
	records # it returns the filtered photo dataset
}


#' Remove records close to aquaria
#'
#' The function will filter records by removing those close to aquaria
#' @param aquaria location data of aquaria
#' @param records location data of records
#' @param max_distance_km distance in kilometers
#' @export 
removeAqua2 <- function(aquaria, dat, max_distance_km = 1){

  library(geosphere)

  # Calculate distances between each record and each aquarium
  calculate_distances <- function(set1, set2) {
    distances <- matrix(0, nrow = nrow(set1), ncol = nrow(set2))
    for (i in 1:nrow(set1)) {
      for (j in 1:nrow(set2)) {
        distances[i, j] <- distVincentySphere(
          set1[i, c("longitude", "latitude")],
          set2[j, c("longitude", "latitude")]
        )
      }
    }
    distances
  }

  # Generate the distance matrix
  dist_matrix <- calculate_distances(set1 = dat, set2 = aquaria)

  # Determine proximity to any aquarium
  is_near_aquarium <- apply(dist_matrix, 1, function(row) any(row < 1000 * max_distance_km))

  # Add the 'aquarium' column to the records dataframe
  dat$aquarium <- is_near_aquarium

  return(dat)
}


#' Remove records close to aquaria - parellelized function
#'
#' The function will filter records by removing those close to aquaria
#' @param aquaria location data of aquaria
#' @param records location data of records
#' @param max_distance_km distance in kilometers
#' @export 
removeAquaPar <- function(aquaria = aqua, records = dat, max_distance_km = 1) {
  # Install and load required packages if not already installed
  # install.packages(c("geosphere", "foreach", "doParallel"))
  library(geosphere)
  library(foreach)
  library(doParallel)

  # Set the number of cores you want to use
  num_cores <- 4  # Change this to the desired number of cores

  # Register parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)

  calculate_distances_parallel <- function(set1, set2) {
    distances <- foreach(i = 1:nrow(set1), .combine = 'cbind') %dopar% {
      dists <- foreach(j = 1:nrow(set2), .combine = 'c') %dopar% {
        distVincentySphere(
          set1[i, c("longitude", "latitude")],
          set2[j, c("longitude", "latitude")]
        )
      }
      return(dists)
    }

    matrix(unlist(distances), nrow = nrow(set1), ncol = nrow(set2))
  }

  distances_parallel <- calculate_distances_parallel(set1 = records, set2 = aquaria)

  # Stop the parallel backend
  stopCluster(cl)

  inaqua.id <- which(distances_parallel < 1000 * max_distance_km, arr.ind = TRUE)[, 1]
  # These are the rows with distance within the aquarium threshold
  filtered_points <- records[-inaqua.id,]

  return(filtered_points)
}






