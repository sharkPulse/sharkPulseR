#' Sanitize sharh names nomenclature
#'
#' convert a list of names containing species names with various frmatting error in a proper list of scientific names
#' @param dat data table with `species_name` column to sanitize
#' @export 
sanitizeTaxonomy = function(dat){
	dat$species_name = sub("^(.)", "\\U\\1", dat$species_name, perl = TRUE) # uppercase the firts letter
	dat$species_name <- sub("(sp|spp|sp\\.|spp\\.)\\b", "", dat$species_name) # remove sp or spp at the end of the names, \\b ensure we match only entire words
# Lowercase the first letter of the second word in each name
	dat$species_name <- gsub("(\\S+) (\\S+)", "\\1 \\L\\2", dat$species_name, perl = TRUE) # lowercase 1st letter of second name
	dat$species_name <- sub("family\\b", "", dat$species_name) # remove the word family - it is obvious when a word is a family name because it ends with idae 
	dat$species_name = trimws(dat$species_name) # remove leading and trailing space
	# when the species_name is one word it can be:
	# a family if it ends in idae
	# an order if it ends in ormes
	# a genus if it does not end with these specific suffixes
	dat
}


