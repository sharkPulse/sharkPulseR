#' Sanitize sharh names nomenclature
#'
#' convert a list of names containing species names with various frmatting error in a proper list of scientific names
#' @param dat data table with `species_name` column to sanitize
#' @export 
sanitizeTaxonomy = function(dat){
	dat$species_name = sub("^(.)", "\\U\\1", dat$species_name, perl = TRUE) # uppercase the firts letter
	dat$species_name <- sub("(species|sp|spp|sp\\.|spp\\.)\\b", "", dat$species_name) # remove sp or spp at the end of the names, \\b ensure we match only entire words
# Lowercase the first letter of the second word in each name
	dat$species_name <- gsub("(\\S+) (\\S+)", "\\1 \\L\\2", dat$species_name, perl = TRUE) # lowercase 1st letter of second name
	dat$species_name <- sub("family\\b", "", dat$species_name) # remove the word family - it is obvious when a word is a family name because it ends with idae 
	dat$species_name <- sub("order\\b", "", dat$species_name) # remove the word order - it is obvious when a word is an order name because it ends with ormes. The word boundary (\\b) ensures that it will only match "order" as a whole word and not as part of another word (like "disorder"). 
	dat$species_name = trimws(dat$species_name) # remove leading and trailing space
	dat$species_name <- gsub("\\s*\\(.*?\\)", "", dat$species_name) # remove parentheses and text within, e.g. "Rhincodon typus (whale shark)"
	# when the species_name is one word it can be:
	# a family if it ends in idae
	# an order if it ends in ormes
	# a genus if it does not end with these specific suffixes
	dat$order <- ifelse(grepl("formes", dat$species_name, ignore.case = TRUE), dat$species_name, "")
# Now replacing 'species_name' entries containing "formes" with a blank
	dat$species_name <- ifelse(grepl("formes", dat$species_name, ignore.case = TRUE), "", dat$species_name)

	dat$family <- ifelse(grepl("idae", dat$species_name, ignore.case = TRUE), dat$species_name, "")
	dat$species_name <- ifelse(grepl("idae", dat$species_name, ignore.case = TRUE), "", dat$species_name)

# creating a genis field with species_name elements of only one word
	dat$genus <- ifelse(sapply(dat$species_name, function(x) length(strsplit(x, "\\s+")[[1]]) == 1), dat$species_name, "")
	dat$species_name <- ifelse(sapply(dat$species_name, function(x) length(strsplit(x, "\\s+")[[1]]) == 1), "", dat$species_name)



	dat
}


