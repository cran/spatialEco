% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.empty.R
\name{is.empty}
\alias{is.empty}
\title{is.empty}
\usage{
is.empty(x, all.na = FALSE, na.empty = TRUE, trim = TRUE)
}
\arguments{
\item{x}{A vector to evaluate elements}

\item{all.na}{(FALSE / TRUE) Return a TRUE if all elements are NA}

\item{na.empty}{(TRUE / FALSE) Return TRUE if element is NA}

\item{trim}{(TRUE / FALSE) Trim empty strings}
}
\value{
A Boolean indicating empty elements in a vector, if all.na = FALSE
a TRUE/FALSE value will be returned for each element in the vector
}
\description{
evaluates empty elements in a vector
}
\details{
This function evaluates if an element in a vector is empty the na.empty argument 
allows for evaluating NA values (TRUE if NA) and all.na returns a TRUE if all elements 
are NA. The trim argument trims a character string to account for the fact that c(" ") 
is not empty but, a vector with c("") is empty. Using trim = TRUE will force both 
to return TRUE
}
\examples{
is.empty( c("") )
is.empty( c(" ") )
is.empty( c(" "), trim=FALSE )

is.empty( c("",NA,1) )
is.empty( c("",NA,1), na.empty=FALSE)

is.empty( c(NA,NA,NA) )
is.empty( c(NA,NA,NA), all.na=TRUE )
is.empty( c(NA,2,NA), all.na=TRUE )

any( is.empty( c("",2,3) ) )
any( is.empty( c(1,2,3) ) )

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
