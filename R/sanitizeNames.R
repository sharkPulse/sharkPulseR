#' Sanitize scientific names
#'
#' convert a list of scientific names into their proper names, cross checking them with a validated list of official names. The function calculates the similarity between the word to check and the words in the reference list and takes teh closest match.
#' @param names species names to sanitize
#' @param refdata data with reference species list
#' @export 
sanitizeNames = function(names, refdata = tx){
require(stringdist)
library(stringr)
require(rfishbase)
refdata$valid = with(refdata, ifelse(is.na(valid),species_name,valid))

# Modify the vector to retain only two and three word strings
names <- ifelse(str_count(names, "\\S+") %in% c(2,3, 4), names, NA) # I need t include also names with three terms because there are some common names - these need to be fixed in the database

unames = unique(na.omit(names)) # this is to avoid computing a huge distance matrix with too many repeated names

dist_sci = stringdistmatrix(unames, refdata$valid, method = "jw")
# Distance matrix for common names
dist_com = stringdistmatrix(unames, refdata$main_common_name, method = "jw")

# Determine the closest scientific and common names
closest_sci_indices <- apply(dist_sci, 1, which.min)
closest_com_indices <- apply(dist_com, 1, which.min)

# Get minimum distances for scientific and common matches
min_sci_distances <- apply(dist_sci, 1, function(x) min(x, na.rm = TRUE))
min_com_distances <- apply(dist_com, 1, function(x) min(x, na.rm = TRUE))

# Classify based on which distance is smaller and retrieve the scientific name if it's closer
classification <- ifelse(min_sci_distances < min_com_distances, "Scientific", "Common")
matched_scientific <- ifelse(min_sci_distances < min_com_distances, refdata$valid[closest_sci_indices], NA)
matched_common <- ifelse(min_com_distances < min_sci_distances, refdata$main_common_name[closest_com_indices], NA)

results <- data.frame(
  Names = unames,
  Classification = classification,
  Matched_Scientific = matched_scientific,
  Matched_Common = matched_common
)

results$final = with(results,ifelse(Names=="","",Matched_Scientific))
#results$final2 = validate_names(results$Names) 3 this is problematic as sometimes the validate_names function resturns two values - we may want to modify this

results$final2 = sapply(results$Names, function(x) {
  validated <- validate_names(x)
  # Exclude "Genus Species" if it appears in the result
  validated <- validated[validated != "Genus Species"]
  if (length(validated) > 1) validated <- validated[1]  # Take the first valid name
  return(validated)
})

results$final = with(results, ifelse(is.na(final2), final, final2)) # this is to retain the validated scientific names when available

minds = match(results$Matched_Common, refdata$main_common_name, nomatch = NA) # find the correspond common name (match indices), when it is 1 it was not a match


results$final = with(results, ifelse(is.na(final),refdata$valid[minds],final))
results$final = with(results, ifelse(final == "Genus Species","",final))


inds = match(names, results$Names) # find indices in results of the species names to change 
names = ifelse(is.na(inds),names,results$final[inds])
names

}


