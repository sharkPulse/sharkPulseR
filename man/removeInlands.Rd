% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeInlands.R
\name{removeInlands}
\alias{removeInlands}
\title{Remove points inland}
\usage{
removeInlands(dat, buffer = 10)
}
\arguments{
\item{dat}{dataset of points}

\item{buffer}{distance to use as a bufer to retain points}
}
\description{
Takes a dataset of observation from Flickr and associated distance from coast. Identifies points inland and remove those > buffer km from coast
}
