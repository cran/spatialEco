% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insert.R
\name{insert}
\alias{insert}
\title{Insert a row or column into a data.frame}
\usage{
insert(x, MARGIN = 1, value = NULL, idx, name = NULL)
}
\arguments{
\item{x}{Existing data.frame}

\item{MARGIN}{Insert a 1 = row or 2 = column}

\item{value}{A vector of values equal to the length of MARGIN,
if nothing specified values with be NA}

\item{idx}{Index position to insert row or column}

\item{name}{Name of new column (not used for rows, MARGIN=1)}
}
\value{
A data.frame with the new row or column inserted
}
\description{
Inserts a new row or column into a data.frame at a specified location
}
\details{
Where there are methods to easily add a row/column to the end or beginning of a data.frame, 
it is not straight forward to insert data at a specific location within the data.frame. 
This function allows for inserting a vector at a specific location eg., between columns or 
rows 1 and 2 where row/column 2 is moved to the 3rd position and a new vector of values is 
inserted into the 2nd position.
}
\examples{
d <- data.frame(ID=1:10, y=runif(10))

# insert row
insert(d, idx=2)
insert(d, value=c(20,0), idx=2)

# insert column
insert(d, MARGIN=2, idx=2)
insert(d, MARGIN = 2, value = rep(0,10), idx=2, name="x")

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
