#' @title sp na.omit
#' @description Removes row or column NA's in sp object
#'
#' @param x Object of class SpatialPointsDataFrame OR SpatialPolygonsDataFrame 
#' @param margin Margin (1,2) of data.frame 1 for rows or 2 for columns
#'
#' @export
#' @note Depends: sp
#'
#' @author Jeffrey S. Evans  <jeffrey_evans<at>tnc.org>
#'                                                                       
#' @examples 
#' require(sp)
#'   data(meuse)
#'     coordinates(meuse) <- ~x+y
#'    
#'    # Display rows with NA  
#'    meuse@@data[!complete.cases(meuse@@data),] 
#'
#'    # Remove NA's in rows (and associated points)
#'    meuse2 <- sp.na.omit(meuse) 
#'      dim(meuse)
#'        dim(meuse2)
#'    
#'    # Plot deleted points in red
#'    plot(meuse, col='red', pch=20)
#'      plot(meuse2, col='black', pch=20, add=TRUE)
#'
#'    # Remove columns with NA's 
#'    meuse2 <- sp.na.omit(meuse, margin=2) 
#'      head(meuse@@data)
#'        head(meuse2@@data)
sp.na.omit <- function(x, margin = 1) {
    if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
        stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT")
    na.index <- unique(as.data.frame(which(is.na(x@data), arr.ind = TRUE))[, margin])
    if (margin == 1) {
        cat("Deleting rows: ", na.index, "\n")
        return(x[-na.index, ])
    }
    if (margin == 2) {
        cat("Deleting columns: ", na.index, "\n")
        return(x[, -na.index])
    }
} 
