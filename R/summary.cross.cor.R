#' @title Summary of spatial cross correlation
#' @description summary method for class "cross.cor"
#'
#' @param object    Object of class cross.cor
#' @param ...       Ignored
#'
#'
#' @return 
#' When not simulated k=0, prints functions list object containing:
#' * I - Global autocorrelation statistic
#' * SCI - - A data.frame with two columns representing the xy and yx autocorrelation
#' * nsim - value of NULL to represent p values were derived from observed data (k=0)
#' * p - Probability based observations above/below confidence interval
#' * t.test - Probability based on t-test
#'
#' When simulated (k>0), prints functions list object containing: 
#'  * I - Global autocorrelation statistic
#'  * SCI - A data.frame with two columns representing the xy and yx autocorrelation
#'  * nsim - value representing number of simulations
#'  * global.p - p-value of global autocorrelation statistic
#'  * local.p - Probability based simulated data using successful rejection of t-test
#'  * range.p - Probability based on range of probabilities resulting from paired t-test
#' @md
#' 
#' @method summary cross.cor
#'
#' @export
summary.cross.cor <- function(object, ...) {
  if(!is.null(object$nsim)) {
  cat("Moran's-I under randomization assumptions...", "\n")
    cat("  First-order Moran's-I: ", object$I, "\n")
  	cat("  First-order p-value: ", object$global.p, "\n")
	cat("", "\n") 
  cat("Chen's SCI under randomization assumptions...", "\n")
	cat("\n", "Summary statistics of local partial cross-correlation [xy]", "\n")
      print( summary(object$SCI[,1]) )
        cat("\n", "", "\n")  
  	cat("  p-value based on 2-tailed t-test: ", object$local.p, "\n")	
    cat("  p-value based on 2-tailed t-test observations above/below CI: ", object$range.p, "\n")	
    if( exists(object$clusters) )
	  cat("\n", "Counts of cluster types")
	    print(table(object$cluster))
  } else {
  cat("Moran's-I...", "\n")
      cat("  First-order Moran's-I: ", object$I, "\n")
  	  cat("  First-order p-value: ", object$global.p, "\n")
	  cat("", "\n") 
  cat("Chen's SCI under randomization assumptions...", "\n")
	cat("\n", "Summary statistics of local partial cross-correlation [xy]", "\n")
      print( summary(object$SCI[,1]) )
	    cat("\n", "", "\n") 
  	cat("    non-simulated second-order p-value based on 2-tailed t-test: ", object$t.test, "\n")
	cat("    p-value based on 2-tailed t-test observations above/below CI: ", object$p, "\n")	
    if( exists(object$clusters) )
	  cat("\n", "Counts of cluster types")
	    print(table(object$cluster))
  }
}
