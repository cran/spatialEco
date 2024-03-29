% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raster.downscale.R
\name{raster.downscale}
\alias{raster.downscale}
\title{Raster Downscale}
\usage{
raster.downscale(
  x,
  y,
  scatter = FALSE,
  full.res = FALSE,
  residuals = FALSE,
  se = FALSE,
  p = 0.95,
  uncertainty = c("none", "prediction", "confidence")
)
}
\arguments{
\item{x}{A terra SpatRaster object representing independent variable(s)}

\item{y}{A terra SpatRaster object representing dependent variable}

\item{scatter}{(FALSE/TRUE) Optional scatter plot}

\item{full.res}{(FALSE/TRUE) Use full resolution of x (see notes)}

\item{residuals}{(FALSE/TRUE) Output raster residual error raster,
at same resolution as y}

\item{se}{(FALSE/TRUE) Output standard error raster, using prediction or
confidence interval}

\item{p}{The confidence/prediction interval (default is 95\%)}

\item{uncertainty}{Output uncertainty raster(s) of confidence or prediction interval,
at same resolution as y. Options are c("none", "prediction", "confidence")}
}
\value{
A list object containing:
\itemize{
\item downscale - downscaled terra SpatRaster object
\item model - MASS rlm model object
\item MSE - Mean Square Error
\item AIC - Akaike information criterion
\item parm.ci - Parameter confidence intervals
\item residuals - If residuals = TRUE, a SpatRaster of the residual error
\item uncertainty - If pred.int = TRUE, SpatRaster's of the lower/upper prediction intervals
\item std.error - If se = TRUE, SpatRaster's of the standard error
}
}
\description{
Downscales a raster to a higher resolution raster using
a robust regression
}
\details{
This function uses a robust regression, fit using an M-estimation with Tukey's biweight
initialized by a specific S-estimator, to downscale a raster based on higher-resolution
or more detailed raster data specified as covariate(s). You can optionally output residual
error, standard error and/or uncertainty rasters. However, please note that when choosing
the type of uncertainty, using a confidence interval (uncertainty around the mean predictions)
when you should be using the prediction interval (uncertainty around a single values) will
greatly underestimate the uncertainty in a given predicted value (Bruce & Bruce 2017).
The full.res = TRUE option uses the x data to sample y rather than y to sample x. THis makes
the problem much more computationally and memory extensive and should be used with caution.
There is also the question of pseudo-replication (sample redundancy) in the dependent variable.
Statistically speaking one would expect to capture the sample variation of x by sampling at the
frequency of y thus supporting the downscaling estimate. Note that if uncertainty is not defined
the prediction interval for standard error defaults to "confidence" else is the same output as
uncertainty (eg., prediction or confidence).
}
\examples{
\donttest{
if (require(geodata, quietly = TRUE)) {
library(terra)
library(geodata)

# Download example data (requires geodata package)
elev <- elevation_30s(country="SWZ", path=tempdir())
slp <- terrain(elev, v="slope")
tmax <- worldclim_country(country="SWZ", var="tmax", path=tempdir())
  tmax <- crop(tmax[[1]], ext(elev))

# Downscale temperature
x=c(elev,slp)
  names(x) <- c("elev","slope")
y=tmax
  names(y) <- c("tmax")

tmax.ds <- raster.downscale(x, y, scatter=TRUE, residuals = TRUE,
                            uncertainty = "prediction", se = TRUE)
	
  # plot prediction and parameters	
  opar <- par(no.readonly=TRUE)
    par(mfrow=c(2,2))
      plot(tmax, main="Temp max")
      plot(x[[1]], main="elevation")
      plot(x[[2]], main="slope")
      plot(tmax.ds$downscale, main="Downscaled Temp max")
  par(opar)

  # Plot residual error and raw prediction +/- intervals
  opar <- par(no.readonly=TRUE)
    par(mfrow=c(2,2))
      plot(tmax.ds$std.error, main="Standard Error")
      plot(tmax.ds$residuals, main="residuals")
      plot(tmax.ds$uncertainty[[1]], 
	       main="lower prediction interval")
      plot(tmax.ds$uncertainty[[2]], 
	       main="upper prediction interval")
  par(opar)
  
  # plot prediction uncertainty
  opar <- par(no.readonly=TRUE)
    par(mfrow=c(2,1))
      plot(tmax.ds$downscale - tmax.ds$uncertainty[[1]], 
	       main="lower prediction interval")
      plot(tmax.ds$downscale - tmax.ds$uncertainty[[2]], 
	       main="upper prediction interval")  
  par(opar)  

} else { 
  cat("Please install geodata package to run example", "\n")
}
}
}
\references{
Bruce, P., & A. Bruce. (2017). Practical Statistics for Data Scientists. O’Reilly Media.
}
\author{
Jeffrey S. Evans  \href{mailto:jeffrey_evans@tnc.org}{jeffrey_evans@tnc.org}
}
