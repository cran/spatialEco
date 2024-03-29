#' @title Dutilleul moving window bivariate raster correlation 
#' @description A bivarate raster correlation using Dutilleul's 
#'              modified t-test
#'       
#' @param x           A terra SpatRaster class object 
#' @param y           A terra SpatRaster class object, same dimensions as x
#' @param d           Distance for finding neighbors
#' @param sample      Apply sub-sampling options; c("none", "random", "hexagonal", "regular") 
#' @param p           If sample != "none", what proportion of population 
#'                    should be sampled
#' @param size        Fixed sample size (default NULL)               
#' 
#'
#' @description 
#' This function provides a bivariate moving window correlation using the modified  
#' t-test to account for spatial autocorrelation. Point based subsampling is provided 
#' for computation tractability. The hexagon sampling is recommended as it it good  
#' at capturing spatial process that includes nonstationarity and anistropy.    
#'
#' @return 
#' A terra SpatRaster or sf POINT class object with the following attributes:
#'   * corr - Correlation 
#'   * Fstat - The F-statistic calculated as degrees of freedom unscaled F-statistic
#'   * p.value - p-value for the test
#'   * moran.x - Moran's-I for x 
#'   * moran.y - Moran's-I for y  
#' @md
#' 
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'                                                                           
#' @references
#' Clifford, P., S. Richardson, D. Hemon (1989), Assessing the significance of the  
#'   correlationbetween two spatial processes. Biometrics 45:123-134.
#' 
#' Dutilleul, P. (1993), Modifying the t test for assessing the correlation between 
#'   two spatial processes. Biometrics 49:305-314. 
#' 
#' @examples
#' \donttest{
#'  p = c("sf", "sp", "terra", "gstat")
#'  if(any(!unlist(lapply(p, requireNamespace, quietly=TRUE)))) { 
#'    m = which(!unlist(lapply(p, requireNamespace, quietly=TRUE)))
#'    message("Can't run examples, please install ", paste(p[m], collapse = " "))
#'  } else {
#'    invisible(lapply(p, require, character.only=TRUE))
#'
#' data(meuse, package = "sp")
#' meuse <- st_as_sf(meuse, coords = c("x", "y"), crs = 28992, 
#'                   agr = "constant") 
#' data(meuse.grid, package = "sp")                                      
#' meuse.grid <- st_as_sf(meuse.grid, coords = c("x", "y"), crs = 28992, 
#'                   agr = "constant") 
#' 
#' ref <- rast(ext(meuse.grid), resolution = 40)
#'   crs(ref) <- crs(meuse)
#' e <- ext(179407.8, 181087.9, 331134.4, 332332.1)
#'                                        
#' # GRID-1 log(copper): 
#' v1 <- variogram(log(copper) ~ 1, meuse) 
#'   x1 <- fit.variogram(v1, vgm(1, "Sph", 800, 1))           
#'   G1 <- krige(zinc ~ 1, meuse, meuse.grid, x1, nmax = 30)
#' G1 <- crop(rasterize(G1, ref, "var1.pred"),e)
#' names(G1) <- "copper"
#'  
#'  # GRID-2 log(elev):
#' v2 <- variogram(log(elev) ~ 1, meuse) 
#'   x2 <- fit.variogram(v2, vgm(1, "Sph", 800, 1))           
#'   G2 <- krige(zinc ~ 1, meuse, meuse.grid, x2, nmax = 30)
#' G2 <- crop(rasterize(G2, ref, "var1.pred"),e)
#' names(G2) <- "elev"
#' 
#' # Raster corrected correlation 
#' acor <- raster.modified.ttest(G1, G2)
#'   plot(acor)
#'  
#' # Sample-based corrected correlation
#' ( cor.hex <- raster.modified.ttest(G1, G2, sample = "hexagonal") )	 
#'   plot(cor.hex["corr"], pch=20)
#' }
#' }
#' @seealso \code{\link[SpatialPack]{modified.ttest}} for test details
#'
#' @export raster.modified.ttest
raster.modified.ttest <- function(x, y, d = "auto", sample = c("none", "random", "hexagonal", "regular"), 
								  p = 0.10, size = NULL) {	
  if(length(find.package("SpatialPack", quiet = TRUE)) == 0)								  
    stop("please install SpatialPack package before running this function")	
  if(missing(x))
    stop("x argument is missing")
  if(!inherits(x, "SpatRaster"))
    stop(deparse(substitute(x)), " must be a terra SpatRast object")	
  if(missing(y))
    stop("y argument is missing")	
  if(!inherits(y, "SpatRaster"))
    stop(deparse(substitute(y)), " must be a terra SpatRast object")
  if(!terra::ext(x) == terra::ext(y))
    stop("Extents do not match")
  if(!any(terra::res(x) == terra::res(y)))
    stop("Resolutions do not match")
  if(sample[1] == "none") {
    if(d == "auto"){
      d = sqrt(2*((terra::res(x)[1]*3)^2))
    } else {
      if(!is.numeric (d)) stop("Distance (d) must be numeric")
    }  
    pts <- sf::st_as_sf( terra::as.points( c(x,y) ) )
      names(pts)[1:2] <- c("x","y")
    nb <- spdep::dnearneigh(pts,0, d)  
    spatial.corr <- data.frame()
      for(i in 1:length(nb)) {
	    if(length(nb[[i]]) > 3) {
		  x.var <- pts$x[nb[[i]]]
		  y.var <- pts$y[nb[[i]]]
        sc <- SpatialPack::modified.ttest(x.var, y.var, 
	            sf::st_coordinates(pts[,1:2][nb[[i]],]), nclass = 1)		
        spatial.corr <- rbind(spatial.corr, round(data.frame(corr = sc$corr, 
		                      Fstat= (sc$dof * sc$Fstat),  
		                      p.value = sc$p.value, moran.x = sc$imoran[1], 
							  moran.y = sc$imoran[2]),5))
        } else {
          spatial.corr <- rbind(spatial.corr, rep(NA, 5))
        }		  							  
      }
	  v <- c("corr", "Fstat", "p.value", "moran.x", "moran.y")  
	  names(spatial.corr) <- v  
        pts <- cbind(pts, spatial.corr)
      s <- terra::rast(lapply(v, function(v) {terra::rasterize(pts, x, field=v)}))	 
	    names(s) <- v
		  terra::crs(s) <- terra::crs(x)
	} else {
	# "random", "hexagonal", "regular"
	  if(!is.null(size)) {
        n = size 
      } else {
        n = round(terra::ncell(x) * p, 0)
      } 
	  e <- as.vector(terra::ext(x))[c(1,3,2,4)] 
      e <- sf::st_as_sfc(sf::st_bbox(c(e[1], e[2], e[3], e[4])))
	  pts <- sf::st_as_sf(sf::st_sample(e, size=n, type=sample[1]))
         pts <- cbind(pts, terra::extract(c(x,y), terra::vect(pts))[,-1])
		   pts <- stats::na.omit(pts)
            sf::st_geometry(pts) <- "geometry"
		names(pts)[1:2] <- c("x","y")   
	    if(d == "auto"){
		  dm <- sf::st_distance(pts)
		    diag(dm) <- NA
              d = sqrt(2*((min(dm, na.rm=TRUE)*3)^2))
        } else {
          if(!is.numeric (d)) stop("Distance (d) must be numeric")
        }	   	   
	  nb <- spdep::dnearneigh(pts,0, d)
	  spatial.corr <- data.frame()	  
        for(i in 1:length(nb)) {
	      if(length(nb[[i]]) > 3) {
		  x.var <- pts$x[nb[[i]]]
		  y.var <- pts$y[nb[[i]]]
          sc <- SpatialPack::modified.ttest(x.var, y.var, 
	              sf::st_coordinates(pts[,1:2][nb[[i]],]), 
	              nclass = 1)		
          spatial.corr <- rbind(spatial.corr, round(data.frame(corr = sc$corr, 
		                        Fstat= (sc$dof * sc$Fstat),  
		                        p.value = sc$p.value, moran.x = sc$imoran[1], 
		  	  				    moran.y = sc$imoran[2]),5))
          } else {
            spatial.corr <- rbind(spatial.corr, rep(NA, 5))
          }		  
        }
	    names(spatial.corr) <- c("corr", "Fstat", "p.value", "moran.x", "moran.y")
          s <- cbind(pts, spatial.corr)
		    s <- s[,-which(names(s) %in% c("x","y"))] 
	  }
  return( s )  
}
