################################################################################
##
## File:    BA2023-HAR-Functions.R
##
## Purpose: HAR functions.
##
## Created: 2016.02.13
##
## Version: 2023.09.15
##
################################################################################

.lag <- 
function(x, lag, start = NA)
{  
  c( rep.int(start, lag), x[1 : (NROW(x) - lag)] )
}
# ------------------------------------------------------------------------------


.meanLags <- 
function(x, lags, start)
{
  ## FUNCTION:
  
  #### Initialize
  ans <- numeric(NROW(x))
  #### Variables
  for (lag in lags)
  {
    ans <- ans + .lag(x = x, lag = lag, start = start)
  }
  #### Answer
  ans / NROW(lags)
}
# ------------------------------------------------------------------------------


.extract.data <- 
function(data, in.dates, const) ## 1, 100 * 252
{
  ## FUNCTION:

  #### Rescaling
  const <- const[1]
  const1 <- 1 / 100
  ind <- c("rv", "bpv", "rvm", "rvp", "rkv")
  data[, ind] <- const * data[, ind]

  #### Auxiliary
  in.dates <- if ( NROW(in.dates) > 0 ) { range(in.dates) }
    else { c(data$date[1], data$date[NROW(data)]) }
  ind <- in.dates[1] <= data$date & data$date <= in.dates[2] 
  rv.mean   <- mean(data$rv[ind])
  data$retcc[is.na(data$retcc)] <- 0
  
  #### Answer
  data.frame(
    date      = data$date,
    rv        = data$rv,
    rv.1      = .lag(data$rv, 1, rv.mean),
    rv.2      = .lag(data$rv, 2, rv.mean),
    rva.1     = .lag(data$rv * (data$retcc < 0), 1, rv.mean / 2),
    rv.1.5    = .meanLags(data$rv, 1 : 5 , rv.mean),
    rv.2.5    = .meanLags(data$rv, 2 : 5 , rv.mean),
    rv.1.22   = .meanLags(data$rv, 1 : 22, rv.mean), 
    rv.6.22   = .meanLags(data$rv, 6 : 22, rv.mean), 
    retcc     = data$retcc,
    check.names = FALSE)      
}
# ------------------------------------------------------------------------------


################################################################################
## LM
################################################################################

.lm.coef <- 
function(fit, 
  tstat = FALSE, name = "")
{
  #### summary
  fit1 <- summary(fit)

  #### which component?
  ind.gam <- class(fit)[1] == "gam"
  ind <- if ( ind.gam ) {"p.table"} else { "coefficients" }
  #### coef
  coef <- fit1[[ind]][, "Estimate"]
  #### tstat
  if (tstat) 
  { 
    #### Compute    
    tstat <- fit1[[ind]][, "t value"] 
    names(tstat) <- paste("t.", names(tstat), sep = "")

    #### Append robust
    if ( !ind.gam )
    {
      #### Newey-West
      x4 <- coeftest(x = fit, vcov. = NeweyWest(x = fit))
      x4 <- x4[, "t value"]
      names(x4) <- paste("tNW.", names(x4), sep = "")
      #### Append
      tstat <- c(tstat, x4)
    }
  } 
  else 
  { 
    tstat <- NULL 
  }
  #### ans
  ans <- c(coef, tstat)
  
  #### Name
  if (name != "")
  {
    names(ans) <- paste(names(ans), name, sep = ".")
  }
  #### Answer
  ans
}
# ------------------------------------------------------------------------------


################################################################################
##
################################################################################

.har.regressors <- 
function(data)
{
  ####
  y.mean <- mean(data$y)
  
  #### Answer
  data.frame(
    date   = data$date,
    y      = data$y,
    y.1    = .lag(data$y, 1, y.mean),
    y.2    = .lag(data$y, 2, y.mean),
    ym.1   = .lag(data$ym, 1, 0.5 * y.mean),
    y.1.5  = .meanLags(data$y, 1 : 5 , y.mean),
    y.2.5  = .meanLags(data$y, 2 : 5 , y.mean),
    y.1.22 = .meanLags(data$y, 1 : 22, y.mean), 
    y.6.22 = .meanLags(data$y, 6 : 22, y.mean), 
    check.names = FALSE) 
}
# ------------------------------------------------------------------------------


.har.fit <- 
function(data)
{
  ## FUNCTION:

  #### Fit
  fit <- lm(data = data, formula = y ~ y.1 + y.2.5 + y.6.22 + ym.1) 

  #### Answer
  list(
    model = list(data = data), 
    fit = fit )
}
# ------------------------------------------------------------------------------


.har.inference <- 
function(fit) 
{
  ## FUNCTION:
  
  #### Extract
  data  <- fit$model$data
  nobs  <- NROW(data$y)
  parNames <- names(fit$model$coefficients)
  parm  <- fit$fit$coefficients
  np <- NROW(parm)

  #### Filter
  mu <- fitted(fit$fit)
  eps <- residuals(fit$fit)
  flt <- cbind(mu = mu, eps = eps)
  #### Further stats
  sfit <- summary(fit$fit)
  #### Sigma
  sigma <- sfit$sigma
  #### Standard errors
  se <- sfit$coefficients[, "Std. Error"]
  #### Loss function
  #### Fit measures
  ## Gaussian log-likelihood
  ll.norm <- -0.5 * nobs * log(2 * pi * sigma^2) -0.5 * sum( eps^2 ) /sigma^2
  ## IC
  ic <- .IC(loglik = ll.norm, np = np, nobs = nobs)
  ## R^2
  R2 <- cor(data$y, flt[, "mu"])^2
  R2adj <- 1 - (1 - R2) * (nobs - 1) / (nobs - np)
  ## Join
  gof <- c(ic, R2 = R2, R2adj = R2adj)
    
  #### Table
  est <- c(parm, sigma = sigma)
  se  <- c(se, sigma = NA)
  table <- data.frame(
    est = est, se = se, zstat = est / se)
  rownames(table) <- names(est)
  
  #### Answer
  list(
    nobs = nobs, gof = gof, sigma = sigma,
    parm = parm,
    vcov = vcov(fit$fit), table = table, flt = flt, 
    model = fit$model, fit = fit$fit)
}
# ------------------------------------------------------------------------------


#### 
.IC <- 
function(loglik, np, nobs)
{
  c(
    AIC = -2 * loglik + 2 * np, 
    AICc = -2 * loglik + 2 * np * (nobs / (nobs - np - 1)), 
    BIC = -2 * loglik + log(nobs) * np)
}
# ------------------------------------------------------------------------------


.har.filter <- 
function(parm, data)
{
  ## FUNCTION:
  
  #### Extract data
  data <- .har.regressors(data)
  ind <- names(parm) %in% intersect(names(parm), colnames(data))
  name <- names(parm)[ ind ]
  X <- cbind(1, data[, name, drop = FALSE])
  colnames(X) <- names(parm)

  #### Fitted
  X <- as.matrix(X)
  flt <- X %*% parm
  
  #### Answer
  data.frame(mu = flt)
}
# ------------------------------------------------------------------------------


.har.indvars1 <- 
function(data)
{
  ####
  y.mean <- mean(data$y)
  
  #### y lagged
  ivy <- matrix(NA, NROW(data$y), 22)
  # colnames(ivy) <- paste0("l", 1 : NCOL(ivy))
  for (l in 1 : 22)
  {
    ivy[, l] <- .lag(data$y, l, y.mean)
  }
  #### ym lagged
  ivym <- .lag(data$ym, 1, 0.5 * y.mean)
  
  #### Answer
  list(y = ivy, ym = ivym)
}
# ------------------------------------------------------------------------------


.har.pred <- 
function(t1, hor, parm, data)
{
  ## FUNCTION:
  
  #### Extract data
  y <- data$y
  x <- .har.indvars1(data = data)
  
  #### Sizes
  nobs <- NROW(y)
  np  <- NROW(parm)

  #### Initialize
  t1 <- t1[1]
  hor <- ifelse(hor[1] <= 1, 1, round(hor))
  xi.store  <- matrix(NA, nobs, hor)
  colnames(xi.store) <- paste0("mu", 1 : NCOL(xi.store))
  
  #### Cycle 1 : (t1 + 1 - hor)
  ind <- if (t1 < nobs) {1 : (t1 - hor)} else {1 : nobs}
  for ( t in ind )
  { 
    #### Aux
    x1 <- x$y[t, ]
    x2 <- x$ym[t]
    x3 <- c(1, x1[1], mean(x1[2:5]), mean(x1[6:22]), x2)

    #### xi[t]
    xi <- sum(parm * x3) 

    #### Store
    xi.store[t, 1] <- xi
  }
  
  #### Cycle t1 : nobs
  ## Aux
  i.x1 <- 1 : NCOL(x$y)
  i.x2 <- 1 : NCOL(x$ym)
  ## Cycle
  ind  <- if (t1 < nobs) {(t1 + 1 - hor) : nobs} else {NULL}
  indh <- if (hor < 2) {NULL} else {2 : hor}
  for ( t in ind )
  { 
    #### Aux
    x1 <- x$y[t, ]
    x2 <- x$ym[t]
    x3 <- c(1, x1[1], mean(x1[2:5]), mean(x1[6:22]), x2)
    
    #### xi[t]    
    xi <- sum(parm * x3) 
    
    #### Store
    xi.store[t, 1] <- xi
    ####
    for (h in indh)
    {  
      #### Update
      x1 <- c(xi, x1)[i.x1]
      x2 <- c(0.5 * xi, x2)[i.x2]
      x3 <- c(1, x1[1], mean(x1[2:5]), mean(x1[6:22]), x2)
      
      #### xi[t]    
      xi <- sum(parm * x3) 
      
      #### Store
      xi.store[t, h] <- xi
    }
  }
    
  #### Answer
  data.frame(.pred.shift(pred = xi.store, t1 = t1, hor = hor))
  # data.frame(xi.store)
  # xi.store
}
# ------------------------------------------------------------------------------


################################################################################
## Manage output
################################################################################

.har.fit.print <- function(fit)
{
  ####
  nobs  <- NROW(fit$model$data$y)
  parm1 <- fit$fit$coefficients
  list( 
    nobs = nobs,
    parm = data.frame(name = names(parm1), parm = parm1) )
}
# ------------------------------------------------------------------------------ 


.har.fit.read <- function(file, symbol, dates)
{
  #### Read
  x1 <- readLines(con = file)
  #### Select block by symbol and dates
  x2 <- .har.fit.symbols.dates(file = file)
  ind <- x2$symbol == symbol & x2$in.first == dates[1] & x2$in.last == dates[2]
  line1 <- x2$line[ind]
  pattern <- "-------------------------------------------"
  ind <- grep(x = x1, pattern = pattern, fixed = TRUE)
  line2 <- min(ind[ind > line1])
  x1 <- x1[line1:line2]

  #### nobs
  pattern <- "$nobs"
  ind <- which( x1 == pattern )
  x2 <- x1[ind+1]
  x2 <- gsub(x = x2, pattern = "[1]", replacement = "", fixed = TRUE)
  x2 <- as.numeric(x2)
  nobs <- x2
  #### parm
  pattern <- "$parm"
  ind1 <- which(x1 == pattern)
  x1 <- x1[(ind1+1):(NROW(x1)-2)]
  x1 <- strsplit(x = x1, split = "[[:blank:]]+")
  x1 <- do.call(what = rbind, args = x1)
  names <- x1[1, ]
  x1 <- x1[-1, ]
  colnames(x1) <- names
  ind <- c("name", "parm")
  x1 <- data.frame(x1[, ind])
  ind <- c("parm")
  x1[, ind] <- mapply(FUN = as.numeric, x = x1[, ind])
  parm <- x1  
  
  #### Answer
  list(parm = parm, nobs = nobs)
}
# ------------------------------------------------------------------------------ 


.har.fit.symbols.dates <- function(file, period.out = NULL)
{
  #### Read
  x1 <- readLines(con = file)
  #### Symbols
  pattern <- "$symbol.dates"
  ind <- which(x1 == pattern)
  x1 <- x1[ind+2]
  x1 <- do.call(what = rbind, args = strsplit(x = x1, split = "[[:blank:]]+"))
  x1 <- data.frame(symbol = x1[, 2], in.first = as.Date(x1[, 3]), 
    in.last = as.Date(x1[, 4])) 
  #### Dates out
  if (NROW(period.out) > 0)
  {
    n1 <- NROW(x1)
    out.last <- rep.int(as.Date("2000-01-01"), n1)
    per <- period.out[1]
    for (i in 1:n1)
    {
      out.last[i] <- seq(from = x1$in.last[i], by = per, length.out = 2)[2]
    }
    x1 <- data.frame(x1, out.first = x1$in.last + 1, out.last = out.last)
  }
  #### Append line numbers
  x1 <- data.frame(x1, line = ind)
  #### Answer
  x1
}
# ------------------------------------------------------------------------------ 

