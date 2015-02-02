#' @title trend.line
#' @description Calculated specified trend line of x,y
#'
#' @param x Vector of x
#' @param y Vector of y
#' @param type   Trend line types are: 'linear', 'exponential', 'logarithmic', 'polynomial'
#' @param plot plot results (TRUE/FALSE)
#' @param ... Additional agruments passed to plot 
#'
#' @export
#' @return A list class object with the following components:
#' for type = 'linear'  x is slope and y is intercept
#' for type = 'exponential', 'logarithmic', or 'polynomial'
#'   x is original x variable and y is vector of fit regression line
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @examples 
#' x <- 1:10
#' y <- jitter(x^2)
#'   opar <- par
#'   par(mfcol=c(2,2))
#'     trend.line(x,y,type='linear',plot=TRUE,pch=20,main='Linear')
#'     trend.line(x,y,type='exponential',plot=TRUE,pch=20,main='Exponential')
#'     trend.line(x,y,type='logarithmic',plot=TRUE,pch=20,main='Logarithmic')
#'     trend.line(x,y,type='polynomial',plot=TRUE,pch=20,main='Polynomial')
#'  par <- opar
#' 
trend.line <- function(x, y, type = "linear", plot = TRUE, ...) {
    if (type == "linear") {
        fit <- glm(y ~ x)
        trend <- c(as.numeric(coef(fit)[1]), as.numeric(coef(fit)[2]))
    }
    if (type == "exponential") {
        exp.f <- function(x, a, b) {
            a * exp(b * x)
        }
        fit <- nls(y ~ exp.f(x, a, b), start = c(a = 1, b = 1))
        trend <- exp.f(x, a = coef(fit)[1], b = coef(fit)[2])
    }
    if (type == "logarithmic") {
        log.f <- function(x, a, b) {
            a * log(x) + b
        }
        fit <- nls(y ~ log.f(x, a, b), start = c(a = 1, b = 1))
        trend <- log.f(x, a = coef(fit)[1], b = coef(fit)[2])
    }
    if (type == "polynomial") {
        poly.f <- function(x, a, b, d) {
            (a * x^2) + (b * x) + d
        }
        fit <- nls(y ~ poly.f(x, a, b, d), start = c(a = 1, b = 1, d = 1))
        trend <- poly.f(x, a = coef(fit)[1], b = coef(fit)[2], d = coef(fit)[3])
    }
    if (plot == TRUE) {
        plot(x, y, ...)
        if (type == "linear") {
            abline(a = trend[1], b = trend[2], col = "black", lwd = 2)
        } else {
            lines(x = x, y = trend, col = "black", lwd = 2)
        }
    }
    if (type == "linear") 
        return(list(x = trend[1], y = trend[2])) else return(list(x = x, y = trend))
} 
