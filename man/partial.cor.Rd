% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partial.cor.R
\name{partial.cor}
\alias{partial.cor}
\title{Partial and Semi-partial correlation}
\usage{
partial.cor(
  x,
  y,
  z,
  method = c("partial", "semipartial"),
  statistic = c("kendall", "pearson", "spearman")
)
}
\arguments{
\item{x}{A vector, data.frame or matrix with 3 columns}

\item{y}{A vector same length as x}

\item{z}{A vector same length as x}

\item{method}{Type of correlation: "partial" or "semipartial"}

\item{statistic}{Correlation statistic, options are: "kendall",
"pearson", "spearman"}
}
\value{
data.frame containing:
\itemize{
\item correlation - correlation coefficient
\item p.value - p-value of correlation
\item test.statistic - test statistic
\item n - sample size
\item Method - indicating partial or semipartial correlation
\item Statistic - the correlation statistic used
}
}
\description{
Calculates a partial or semi-partial correlation
with parametric and nonparametric options
}
\details{
Partial and semipartial correlations show the association between two
variables when one or more peripheral variables are controlled
to hold them constant.

Suppose we have three variables, X, Y, and Z. Partial correlation holds
constant one variable when computing the relations two others. Suppose we
want to know the correlation between X and Y holding Z constant for both
X and Y. That would be the partial correlation between X and Y controlling
for Z. Semipartial correlation holds Z constant for either X or Y, but not
both, so if we wanted to control X for Z, we could compute the semipartial
correlation between X and Y holding Z constant for X.
}
\examples{
air.flow = stackloss[,1]
water.temperature = stackloss[,2]
acid = stackloss[,3]

# Partial using Kendall (nonparametric) correlation
partial.cor(air.flow, water.temperature, acid)

scholar <- data.frame(
  HSGPA=c(3.0, 3.2, 2.8, 2.5, 3.2, 3.8, 3.9, 3.8, 3.5, 3.1), 
	 FGPA=c(2.8, 3.0, 2.8, 2.2, 3.3, 3.3, 3.5, 3.7, 3.4, 2.9),
  SATV =c(500, 550, 450, 400, 600, 650, 700, 550, 650, 550)) 

# Standard Pearson's correlations between HSGPA and FGPA  
cor(scholar[,1], scholar[,2])

# Partial correlation using Pearson (parametric) between HSGPA 
#   and FGPA, controlling for SATV
partial.cor(scholar, statistic="pearson")

# Semipartial using Pearson (parametric) correlation 
partial.cor(x=scholar[,2], y=scholar[,1], z=scholar[,3], 
            method="semipartial", statistic="pearson")

}
\author{
Jeffrey S. Evans  \href{mailto:jeffrey_evans@tnc.org}{jeffrey_evans@tnc.org}
}
