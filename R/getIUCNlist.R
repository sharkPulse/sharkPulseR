#' Get the complete list of Chondrichthyes from the IUCN
#'
#' It uses the rredlist package to compile the complete list of cartilaginous fishes in the IUCN Red List of Threatened species 
#' @param key is the IUCN Red List key
#' @export 
getIUCNlist = function(key = "7532c24e6a4d09f24d595f4925beaaf65a9dd07e4ee31fb1c51dece2991df213"){
	require(rredlist)
	IUCN_REDLIST_KEY <- key
	out <- rl_sp(all = TRUE, key = IUCN_REDLIST_KEY)
	all_df <- do.call(rbind, lapply(out, "[[", "result")) #concatenates all data frames in the list
	chond = all_df[all_df$class_name=="CHONDRICHTHYES",]
	chond$superorder = with(chond, ifelse(order_name %in% c("CARCHARHINIFORMES","HETERODONTIFORMES","HEXANCHIFORMES","LAMNIFORMES","ORECTOLOBIFORMES","PRISTIOPHORIFORMES","SQUALIFORMES","SQUATINIFORMES"), "Selachimorpha", ifelse(order_name == "CHIMAERIFORMES", "Holocephalimorpha", "Batoidea")))

	# includes a superorder to divide sharks and rays

	write.csv(chond, file = "../data/allChondrichthyes.csv", row.names = F) # saved on Oct 24, 2023
	chond
}