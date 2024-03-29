% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shift.R
\name{shift}
\alias{shift}
\title{shift}
\usage{
shift(x, lag = 1, pad = NA)
}
\arguments{
\item{x}{A vector}

\item{lag}{Number of lagged offsets, default is 1}

\item{pad}{Value to fill the lagged offset with, default is NA}
}
\value{
A vector, length equal to x, with offset length filled with pad values
}
\description{
Shift a vector by specified positive or negative lag
}
\examples{
x <- 1:10

shift(x, 1)     # shift positive (from beginning of vector) by 1
shift(x, -1)    # shift negative (from end of vector) by 1
shift(x, 5, 0)  # Shift by 5 and fill (pad) with 0

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
