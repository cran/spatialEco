#' @title Landscape metrics for points and polygons
#' @description Calculates a variety of landscape metrics, on binary rasters, for polygons or points with a buffer distance 
#'
#' @param x          SpatalPointsDataFrame or SpatalPolgonsDataFrame class object 
#' @param y          raster class object (binary raster)
#' @param bkgd       Background value
#' @param metrics    Numeric index of desired metric (see available metrics)
#' @param bw         Buffer distance (ignored if x is SpatalPolgonsDataFrame) 
#' @param latlon     Is raster data in lat-long (TRUE/FALSE)
#' @param Trace      Plot raster subsets and echo object ID at each iteration (TRUE | FALSE)
#'
#' @return data.frame with specified metrics in columns. The data.frame is ordered the same as the input feature class and can be directly joined to the @@data slot   
#'
#' @note
#'  [1]class, [2]n.patches, [3]total.area, [4]prop.landscape          
#'  [5]patch.density, [6]total.edge, [7]edge.density, [8]landscape.shape.index   
#'  [9]largest.patch.index, [10]mean.patch.area, [11]sd.patch.area, [12]min.patch.area          
#'  [13]max.patch.area, [14]perimeter.area.frac.dim, [15]mean.perim.area.ratio, [16]sd.perim.area.ratio     
#'  [17]min.perim.area.ratio, [18]max.perim.area.ratio, [19]mean.shape.index, [20]sd.shape.index          
#'  [21]min.shape.index, [22]max.shape.index, [23]mean.frac.dim.index, [24]sd.frac.dim.index       
#'  [25]min.frac.dim.index, [26]max.frac.dim.index, [27]total.core.area, [28]prop.landscape.core     
#'  [29]mean.patch.core.area, [30]sd.patch.core.area, [31]min.patch.core.area, [32]max.patch.core.area     
#'  [33]prop.like.adjacencies, [34]aggregation.index, [35]lanscape.division.index, [36]splitting.index         
#'  [37]effective.mesh.size, [38]patch.cohesion.index 
#' 
#' @note depends: sp, raster, rgeos, SDMTools 
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' library(raster)
#' library(sp)
#' r <- raster(ncol=1000, nrow=1000)
#'   r[] <- rpois(ncell(r), lambda=1)
#'     r <- calc(r, fun=function(x) { x[x >= 1] <- 1; return(x) } )  
#' x <- sampleRandom(r, 10, na.rm = TRUE, sp = TRUE)
#' 
#' land.metrics(x=x, y=r, bw=0.72, bkgd = 0, latlon = TRUE, Trace = FALSE)
#'
#' @export 
land.metrics <- function(x, y, bkgd = NA, metrics = c(4, 14, 33, 34, 35, 37, 38), bw = 1000, latlon = FALSE, Trace = TRUE) {
    if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
        stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT")
    if (!inherits(y, "RasterLayer")) 
        stop("MUST BE raster CLASS OBJECT")
    mnames <- c("class", "n.patches", "total.area", "prop.landscape", "patch.density", "total.edge", "edge.density", 
        "landscape.shape.index", "largest.patch.index", "mean.patch.area", "sd.patch.area", "min.patch.area", "max.patch.area", 
        "perimeter.area.frac.dim", "mean.perim.area.ratio", "sd.perim.area.ratio", "min.perim.area.ratio", "max.perim.area.ratio", 
        "mean.shape.index", "sd.shape.index", "min.shape.index", "max.shape.index", "mean.frac.dim.index", "sd.frac.dim.index", 
        "min.frac.dim.index", "max.frac.dim.index", "total.core.area", "prop.landscape.core", "mean.patch.core.area", 
        "sd.patch.core.area", "min.patch.core.area", "max.patch.core.area", "prop.like.adjacencies", "aggregation.index", 
        "lanscape.division.index", "splitting.index", "effective.mesh.size", "patch.cohesion.index")
    results <- as.data.frame(array(0, dim = c(0, length(metrics))))
    names(results) <- mnames[metrics]
    for (j in 1:dim(x)[1]) {
        if (Trace == TRUE) 
            cat("Processing OBSERVATION -", j, "\n")
        lsub <- x[j, ]
        if (class(x) == "SpatialPointsDataFrame") {
            f <- rgeos::gBuffer(lsub, width = bw, joinStyle = "ROUND", quadsegs = 10)
			fext <- methods::as(raster::extent(f), "SpatialPolygons") 
            cr <- raster::crop(y, fext, snap = "out")
            crop.NA <- raster::setValues(cr, NA)
            fr <- raster::rasterize(f, cr)
            lr <- raster::mask(x = cr, mask = fr)
        }
        if (class(x) == "SpatialPolygonsDataFrame") {
            cr <- raster::crop(y, raster::extent(lsub), snap = "out")
            crop.NA <- raster::setValues(cr, NA)
            fr <- raster::rasterize(lsub, cr)
            lr <- raster::mask(x = cr, mask = fr)
        }
        if (Trace == TRUE) {
            graphics::plot(lr, main = paste("Feature class", j, sep = " - "))
        }
        LM <- SDMTools::ClassStat(lr, cellsize = raster::res(cr)[1], bkgd = bkgd, latlon = latlon)[metrics]
        if (class(LM) == "NULL") {
            LM <- results[1, ]
            LM[1, ] <- rep(NA, dim(results)[2])
            names(LM) <- names(results)
        }
        row.names(LM) <- row.names(lsub@data)
        results <- rbind(results, LM[1, ])
    }
    return(results)
} 
