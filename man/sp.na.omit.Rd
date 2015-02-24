% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/sp.na.omit.R
\name{sp.na.omit}
\alias{sp.na.omit}
\title{sp na.omit}
\usage{
sp.na.omit(x, margin = 1)
}
\arguments{
\item{x}{Object of class SpatialPointsDataFrame OR SpatialPolygonsDataFrame}

\item{margin}{Margin (1,2) of data.frame 1 for rows or 2 for columns}
}
\description{
Removes row or column NA's in sp object
}
\note{
Depends: sp
}
\examples{
require(sp)
  data(meuse)
    coordinates(meuse) <- ~x+y

   # Display rows with NA
   meuse@data[!complete.cases(meuse@data),]

   # Remove NA's in rows (and associated points)
   meuse2 <- sp.na.omit(meuse)
     dim(meuse)
       dim(meuse2)

   # Plot deleted points in red
   plot(meuse, col='red', pch=20)
     plot(meuse2, col='black', pch=20, add=TRUE)

   # Remove columns with NA's
   meuse2 <- sp.na.omit(meuse, margin=2)
     head(meuse@data)
       head(meuse2@data)
}
\author{
Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
}
