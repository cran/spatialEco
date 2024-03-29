% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remove_duplicates.R
\name{remove_duplicates}
\alias{remove_duplicates}
\title{Remove duplicate geometries}
\usage{
remove_duplicates(x, threshold = 0.00001)
}
\arguments{
\item{x}{An sf POINT, POLYGON or LINESTRING object}

\item{threshold}{A distance threshold indicating fuzzy duplication,
default i 0.00001}
}
\value{
sf object, of same feature class as x, with duplicate geometries removed
}
\description{
Removes duplicate geometries in a single-part feature class
}
\details{
This function removes duplicate geometries based on order and not "non null" 
attribution or other factors, the first feature gets to stay. If one needs to 
know which points were removed sf::st_difference can be used between original 
data and results of the function.
}
\examples{
library(sf)

# data with 10 duplicate obs
s <- data.frame(x = runif(100), y = runif(100))
  s <- data.frame(rbind(s, s[sample(1:nrow(s), 10),]) ) 
    s <- st_as_sf(s, coords = c("x", "y"))
      s$ID <- 1:nrow(s)

nrow(s) 
nrow( srmd <- remove_duplicates(s) )

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
