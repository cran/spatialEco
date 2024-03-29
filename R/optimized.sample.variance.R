#' @title Optimized sample variance 
#' @description 
#' Draws an optimal sample that minimizes or maximizes the sample variance 
#'
#' @param x       A vector to draw a sample from
#' @param n       Number of samples to draw
#' @param type    Type of sample variance optimization 
#'                c("maximized", "minimized")  
#'
#' @return 
#' A data.frame with "idx" representing the index of the original vector  
#' and "y" is the value of the sampled data
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples
#' library(sf)
#' if (require(sp, quietly = TRUE)) {
#'   data(meuse, package = "sp")
#'   meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, agr = "constant")
#'	
#'  n = 15
#'  # Draw n samples that maximize the variance of y
#'  ( max.sv <- optimized.sample.variance(meuse$zinc, 15) )
#'  
#'  # Draw n samples that minimize the variance of y
#'  ( min.sv <- optimized.sample.variance(meuse$zinc, 15, 
#'                type="minimized") )
#'  
#'  # Plot results
#'  plot(st_geometry(meuse), pch=19, col="grey")
#'    plot(st_geometry(meuse[max.sv$idx,]), col="red", add=TRUE, pch=19)
#'      plot(st_geometry(meuse[min.sv$idx,]), col="blue", add=TRUE, pch=19)
#'  	  box()
#'      legend("topleft", legend=c("population","maximized variance", 
#'             "minimized variance"), col=c("grey","red","blue"),  
#'             pch=c(19,19,19))  
#'  
#' } else { 
#'   cat("Please install sp package to run example", "\n")
#' }
#' @export optimized.sample.variance  
optimized.sample.variance <- function(x, n, type = "maximized") {
  if(!is.numeric(x)) stop("x is not a numeric vector")
    non.na.idx <- which(!is.na(x))
      if(length(non.na.idx) != length(x)) {
	    x <- stats::na.omit(x)
	    names(x) <- non.na.idx
	  } else {
        names(x) <- 1:length(x) 
      }	  
    y <- idx <- rep(NA, n)
    mean.y <- mean(x)
      for(i in 1:n){
	    if(type == "maximized") {  
          opt <- which.max((x - mean.y)^2)
	    } else if (type == "minimized") {
          opt <- which.min((x - mean.y)^2)		
        } else {
	      stop("Not a valid optimization option")
        }
        idx[i] <- as.numeric(names(opt))
        y[i] <- x[opt]
        x[opt] <- NA
      mean.y <- mean(y, na.rm=TRUE)
      }
    message("\n", paste0(type, " sample variance: "), stats::var(y), "\n")
  return( data.frame(idx=idx,y=y) )
}
