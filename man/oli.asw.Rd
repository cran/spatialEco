% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oli.aws.R
\name{oli.asw}
\alias{oli.asw}
\title{Query AWS-OLI}
\usage{
oli.asw(path, row, dates, cloud.cover = 10, processing)
}
\arguments{
\item{path}{landsat path}

\item{row}{landsat row}

\item{dates}{dates, single or start-stop range in YYYY-MM-DD format}

\item{cloud.cover}{percent cloud cover}

\item{processing}{processing level ("L1GT" or "L1T")}
}
\value{
data.frame object with:
\itemize{ 
\item entityId -  Granule ID 
\item      L = Landsat
\item      X = Sensor
\item      SS = Satellite
\item      PPP = WRS path
\item      RRR = WRS row
\item      YYYYMMDD = Acquisition date
\item      yyyymmdd = Processing date
\item      CC = Collection number
\item      TX = Collection category
\item acquisitionDate - POSIXct YYYY-MM-DD (eg., 2015-01-02)
\item cloudCover -  % cloud cover in whole numbers (0-100)
\item processingLevel - USGS processing level
\item path - Landsat path
\item row - Landsat row
}
}
\description{
Query of Amazon AWS OLI-Landsat 8 cloud service
}
\details{
Amazons AWS cloud service is hosting OLI Landsat 8 data granules
  \url{https://registry.opendata.aws/landsat-8}
  \url{https://aws.amazon.com/blogs/aws/start-using-landsat-on-aws/}

USGS Landsat collections: \url{https://www.usgs.gov/landsat-missions}
Pre-collection processing levels: "L1T", "L1GT", "L1G"
Collection 1 processing levels: "L1TP", "L1GT", "L1GS"
    "L1T" and "L1TP" - Radiomertically calibrated and orthorectified (highest level processing) 
    "L1GT" and "L1GT" - Radiomertically calibrated and systematic geometric corrections   
    "L1G" and "L1GS" - Radiomertically calibrated with systematic ephemeris correction
}
\examples{
\donttest{
# Query path 126, row 59, 2013-04-15 to 2017-03-09, <20\% cloud cover    
( p126r59.oli <- oli.asw(path=126, row=59, dates = c("2013-04-15", "2017-03-09"), 
                          cloud.cover = 20) )

# Download images from query
 bands <- c("_B1.TIF", "_B2.TIF", "_B3.TIF", "_B4.TIF", "_B5.TIF", 
            "_B6.TIF","_B7.TIF", "_B8.TIF", "_B9.TIF", "_B10.TIF",
         "_B11.TIF", "_BQA.TIF","_MTL.txt") 
  for(i in 1:length(p126r59.oli$download_url)) {
    oli.url <- gsub("/index.html","",p126r59.oli$download_url[i])
 all.bands <- paste(oli.url, paste0(unlist(strsplit(oli.url, "/"))[8], bands), sep="/")
   for(j in all.bands) {  
        try(utils::download.file(url=j, destfile=basename(j), mode = "wb"))
       }		 
  }
}

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
