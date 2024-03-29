#' @title Time to event
#' @description Returns the time (sum to position) to a specified value 
#'       
#' @param x          A vector, representing time-series, to evaluate 
#' @param y          Threshold value tor return position for
#' @param dir        Direction of evaluation c("LR", "RL")
#' @param int        FALSE | TRUE - Evaluate as integer (rounds to 0 decimal places)
#' @param up.to      FALSE | TRUE - Return value before event
#' @param na.action  c("fail", "ignore"), if "fail" function will return error with NA's
#'                   with "ignore" NA values will be included in count to event
#'
#' @details
#' The time to event represents the sum of positions, in the vector,
#' until the specified value is found ie., (0,0,1) would be 3 or, 
#' 2 with up.to=TRUE. The int argument allows for rounding a continuous  
#' variable. Since it may be difficult to find an exact match to a floating 
#' point value rounding mitigates the problem. If you want a specific rounding 
#' value (eg., 1 decimal place) you can apply it to x first then pass it to 
#' the function. The up.to argument will stop one value before the specified value 
#' of (y) regardless of integer or float. For NA handling, na.action defines the
#' function behavior, causing it to fail or count NAs. Note that it makes no
#' sense to actually remove NAs as it will make the run uninterpretable.    
#'
#' @return A vector value representing the time to event 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> 
#'
#' @examples
#' # Binomial instance
#' time_to_event(c(0,0,0,0,1,0,0,0,1,0))
#' time_to_event(c(0,0,0,0,1,0,0,0,1,0), up.to = TRUE)
#' time_to_event(c(0,0,0,0,1,0,0,0,1,0), dir="RL")
#' time_to_event(c(NA,0,0,0,1,0,0,0,1,0), na.action="ignore")
#' 
#' # Continuous threshold instance
#' ( x <- runif(100, 0,7) ) 
#' time_to_event(x, y = 5, int=TRUE)
#' 
#' # raster example
#' library(terra)
#'
#' # Binomial instance
#' r <- do.call(c, replicate(20,terra::rast(matrix(sample(
#'              c(0,1), 1000, replace=TRUE), 100, 100))))             
#'   ( t2e <- app(r, fun=time_to_event) )
#' 
#' # Continuous threshold instance
#' r <- do.call(c, replicate(20,terra::rast(matrix(
#'               runif(1000,0,7), 100, 100))))
#'   ( t2e <- app(r, fun=time_to_event, y=5) )
#'
#' @export
time_to_event <- function(x, y = 1, dir = c("LR", "RL"), int = FALSE,
                          up.to = FALSE, na.action=c("fail","ignore")) {
	if(length(which(x >= y)) < 1)
	  stop("y does not exist in data")  
	if(any(is.na(x))) {
	  if(na.action[1] == "fail")
	    stop("NA's present in data")
	  if(y == 0) { 
        x[is.na(x)] <- 1	
	  } else {
	    s <- stats::na.omit(unique(x[x != y]))
		x[is.na(x)] <- sample(s[s < y], 1)
	  }
	}	
    if(dir[1] == "RL") x <- rev(x)
	  if(int) x <- round(x,0)
        x.idx <- rle(x)$values
        e.idx <- rle(x)$lengths 
    if(x[1] == y) {
      if(up.to) e = 0 else e = 1
    } else {
	  e <- sum(e.idx[1:(which(x.idx >= y)[1]-1)])
    } 
  if(up.to) return(e) else return( e + 1 )
}
