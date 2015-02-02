#' @title Logistic and Auto-logistic regression
#' @description Performs a logistic (binomial) and auto-logistic (spatially lagged binomial) regression
#'
#' @export
#' @param ldata data.frame object contaning variables
#' @param y Dependent variable (y) in ldata
#' @param x Independent variable(s) (x) in ldata  
#' @param penalty Apply regression penelty (TRUE/FALSE)
#' @param autologistic Add auto-logistic term (TRUE/FALSE)  
#' @param coords Geographic coordinates for auto-logistc model
#' @param bw Distance bandwidth to calculate spatial lags (if empty neighbors result, need to increase bandwith)
#' @param type Neighbor weighting scheme (see autocov_dist)
#' @param style Type of neighbor matrix (Wij), default is mean of neighbors 
#' @param longlat Are coordinates (coords) in geographic, lat/long (TRUE/FALSE)
#' @param ... Additional arguments passed to lrm
#'
#' @return A list class object with the following components: 
#' @return     model lrm model object (rms class)
#' @return     diagTable data.frame of regression diagnostics
#' @return     coefTable data.frame of regression coefficents
#' @return     Residuals data.frame of residuals and standardized residuals
#' @return     AutoCov If an auto-logistic model, AutoCov represents lagged auto-covariance term
#'
#' @note Spatially lagged y defined as:  
#' @note   W(y)ij=sumj_(Wij yj)/ sumj_(Wij)
#' @note     where; Wij=1/Euclidian[i,j]
#'
#' @note depends: rms, spdep
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'   
#' @references
#' Le Cessie S, Van Houwelingen JC (1992) Ridge estimators in logistic regression. Applied Statistics 41:191-201
#' @references
#' Shao J (1993) Linear model selection by cross-validation. JASA 88:486-494
#'
#' @examples
#' require(sp)
#' require(spdep)
#' require(rms)                                                                       
#' data(meuse)
#'   coordinates(meuse) <- ~x+y  
#'     meuse@@data <- data.frame(DepVar=rbinom(dim(meuse)[1], 1, 0.5), meuse@@data)
#'
#' #### Logistic model
#' lmodel <- logistic.regression(meuse@@data, y='DepVar', x=c('dist','cadmium','copper')) 
#'   lmodel$model
#'     lmodel$diagTable
#'       lmodel$coefTable
#' 
#' ### Auto-logistic model using 'autocov_dist' in 'spdep' package
#' lmodel <- logistic.regression(meuse@@data, y='DepVar', x=c('dist','cadmium','copper'),
#'                               autologistic=TRUE, coords=coordinates(meuse), bw=5000) 
#'   lmodel$model
#'     lmodel$diagTable
#'       lmodel$coefTable
#'   est <- predict(lmodel$model, type='fitted.ind')
#' 
#' #### Add residuals, standardized residuals and estimated probabilities
#' VarNames <- rownames(lmodel$model$var)[-1]
#'   meuse@@data$AutoCov <- lmodel$AutoCov
#'     meuse@@data <- data.frame(meuse@@data, Residual=lmodel$Residuals[,1], 
#'                              StdResid=lmodel$Residuals[,2], Probs=predict(lmodel$model, 
#'                              meuse@@data[,VarNames],type='fitted') )  
#' 
#' #### Plot fit and probabilities
#' resid(lmodel$model, "partial", pl="loess") 
#' resid(lmodel$model, "partial", pl=TRUE)                 # plot residuals    
#' resid(lmodel$model, "gof")                              # global test of goodness of fit
#' lp1 <- resid(lmodel$model, "lp1")                       # Approx. leave-out linear predictors 
#' -2 * sum(meuse@@data$DepVar * lp1 + log(1-plogis(lp1))) # Approx leave-out-1 deviance
#' spplot(meuse, c('Probs'))                               # plot estimated probs at points
#'
logistic.regression <- function(ldata, y, x, penalty = TRUE, autologistic = FALSE, coords = NULL, bw = NULL, type = "inverse", 
    style = "W", longlat = FALSE, ...) {
    if (is.na(match(y, names(ldata)))) 
        stop("Dependent variable not present in data")
    xNames <- intersect(x, names(ldata))
    if (length(xNames) < length(x)) 
        stop("Mismatch in Independent Variable Names")
    if (autologistic == TRUE) {
        if (is.null(coords)) 
            stop("Need coordinates")
        if (is.null(bw)) 
            stop("Need distance bandwidth")
        ldata$AutoCov <- spdep::autocov_dist(ldata$DepVar, xy = coords, nbs = bw, style = style, type = type)
        x <- append(x, "AutoCov")
    }
    form <- as.formula(paste(y, paste(x, collapse = "+"), sep = "~"))
    fit <- rms::lrm(form, data = ldata, x = TRUE, y = TRUE, ...)
    bf <- rms::pentrace(fit, seq(0.2, 1, by = 0.05))
    if (penalty) {
        pen <- bf$penalty
    } else {
        pen <- 0
    }
    allPens <- bf$results.all[, 1]
    allAICs <- bf$results.all[, 3]
    for (i in 1:length(allPens)) {
        penValue <- allPens[i]
        if (penValue == pen) {
            aic <- allAICs[i]
        }
    }
    if (penalty) {
        fit <- update(fit, penalty = bf$penalty)
    }
    res <- residuals(fit)
    resSTD <- (res - mean(res))/sqrt(var(res))
    allIndVars <- c("Intercept")
    allIndVars <- append(allIndVars, x)
    k <- length(allIndVars)
    d <- matrix(0, k, 4)
    d[, 1] <- fit$coefficients
    d[, 2] <- sqrt(diag(fit$var))
    d[, 3] <- d[, 1]/d[, 2]
    d[, 4] <- pnorm(abs(d[, 3]), lower.tail = FALSE) * 2
    coefList <- list(Variable = allIndVars, Coef = d[, 1], StdError = d[, 2], Wald = d[, 3], Prob = d[, 4])
    coefFrame <- data.frame(coefList)
    diagFrame <- data.frame(Names = c(names(fit$stats), "PEN", "AIC"), Value = c(as.vector(fit$stats), pen, aic))
    if (autologistic == TRUE) {
        return(list(model = fit, diagTable = diagFrame, coefTable = coefFrame, Residuals = data.frame(res = res, 
            resSTD = resSTD), AutoCov = ldata$AutoCov))
    } else {
        return(list(model = fit, diagTable = diagFrame, coefTable = coefFrame, Residuals = data.frame(res = res, 
            resSTD = resSTD)))
    }
} 
