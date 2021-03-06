% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/date_seq.R
\name{date_seq}
\alias{date_seq}
\title{date sequence}
\usage{
date_seq(
  start,
  end,
  step = c("day", "week", "month", "quarter", "year", "minute"),
  rm.leap = FALSE
)
}
\arguments{
\item{start}{Start date in "yyyy/mm/dd" character format}

\item{end}{End date in "yyyy/mm/dd" character format}

\item{step}{Time step, options are c("day", "week", "month", "quarter", 
"year", "minute")}

\item{rm.leap}{Remove extra days in leap years}
}
\value{
A date vector of class POSIXct for minute and Date for other options
}
\description{
creates date sequence given start and stop dates
}
\examples{
# monthly steps 1990/01/01 - 2019/12/31
d <- date_seq("1990/01/01", "2019/12/31", step="month")

# daily steps 1990/01/01 - 2019/12/31
d <- date_seq("1990/01/01", "2019/12/31", step="day")

# daily steps 1990/01/01 - 2019/12/31 with leap days removed
d <- date_seq("1990/01/01", "2019/12/31", step="day", rm.leap=TRUE)

# daily step 2008/12/29 - 2008/12/31, 2008 is leap year
d <- date_seq("2008/12/29", "2008/12/31")

# minutes step 2008/12/29 - 2008/12/31, 2008 is leap year
d <- date_seq("2008/12/29", "2008/12/31", step="minute")

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org>
}
