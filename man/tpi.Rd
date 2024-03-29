% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tpi.R
\name{tpi}
\alias{tpi}
\title{Topographic Position Index (tpi)}
\usage{
tpi(x, scale = 3, win = "rectangle", normalize = FALSE, zero.correct = FALSE)
}
\arguments{
\item{x}{A terra SpatRaster object}

\item{scale}{focal window size (n-cell x n-cell for rectangle or 
distance for circle)}

\item{win}{Window type. Options are "rectangle" and "circle"}

\item{normalize}{Apply deviation correction that normalizes to local 
surface roughness}

\item{zero.correct}{Apply correction for zero values in matrix weights}
}
\value{
A terra SpatRaster object of tpi A terra SpatRaster object
}
\description{
Calculates topographic position using mean deviations
}
\examples{
\donttest{
library(terra)
elev <- rast(system.file("extdata/elev.tif", package="spatialEco"))

# calculate tpi and plot 
  tpi7 <- tpi(elev, scale=7) 
  tpi025 <- tpi(elev, win = "circle", scale=2500)
  tpi025.zc <- tpi(elev, win = "circle", scale=2500, 
                   zero.correct = TRUE)

opar <- par(no.readonly=TRUE)
    par(mfrow=c(2,2))
      plot(elev, main="original raster")
      plot(tpi7, main="tpi 7x7")
      plot(tpi025, main="tpi Circular window d=2500m")
   plot(tpi025, main="tpi Circular window d=2500m, zero correct")
par(opar)
}

}
\references{
De Reu, J., J. Bourgeois, M. Bats, A. Zwertvaegher, V. Gelorini, et al., (2014) 
  Application of the topographic position index to heterogeneous landscapes. 
  Geomorphology, 186:39-49.
}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
