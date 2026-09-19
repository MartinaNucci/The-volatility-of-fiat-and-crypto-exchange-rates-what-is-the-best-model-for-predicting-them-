################################################################################
##
## File:    BA2023-naive-Functions.R
##
## Purpose: Naive functions.
##
## Created: 2023.10.02
##
## Version: 2023.10.02
##
################################################################################


################################################################################
##
################################################################################

.naive.regressors <- 
function(data)
{
  ####
  y.mean <- mean(data$y)
  
  #### Answer
  data.frame(
    date   = data$date,
    y      = data$y,
    check.names = FALSE) 
}
# ------------------------------------------------------------------------------


.naive.fit <- 
function(data)
{
  ## FUNCTION:

  #### Fit
  fit <- lm(data = data, formula = y ~ 1) 

  #### Answer
  list(
    model = list(data = data), 
    fit = fit )
}
# ------------------------------------------------------------------------------


.naive.inference <- 
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


.naive.filter <- 
function(parm, data)
{
  ## FUNCTION:
  
  #### Extract data
  data <- .naive.regressors(data)
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


.naive.pred <- 
function(t1, hor, parm, data)
{
  ## FUNCTION:
  
  #### Extract data
  y <- data$y
  
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
    #### xi[t]
    xi <- parm 

    #### Store
    xi.store[t, 1] <- xi
  }
  
  #### Cycle t1 : nobs
  ## Cycle
  ind  <- if (t1 < nobs) {(t1 + 1 - hor) : nobs} else {NULL}
  indh <- if (hor < 2) {NULL} else {2 : hor}
  for ( t in ind )
  {     
    #### xi[t]    
    xi <- parm 
    
    #### Store
    xi.store[t, 1] <- xi
    ####
    for (h in indh)
    {  
      #### xi[t]    
      xi <- parm 
      
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

.naive.fit.print <- function(fit)
{
  ####
  nobs  <- NROW(fit$model$data$y)
  parm1 <- fit$fit$coefficients
  list( 
    nobs = nobs,
    parm = data.frame(name = names(parm1), parm = parm1) )
}
# ------------------------------------------------------------------------------ 


.naive.fit.read <- function(file, symbol, dates)
{
  #### Read
  x1 <- readLines(con = file)
  #### Select block by symbol and dates
  x2 <- .naive.fit.symbols.dates(file = file)
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
  print(nobs)
  #### parm
  pattern <- "$parm"
  ind1 <- which(x1 == pattern)
  x1 <- x1[(ind1+1):(NROW(x1)-1)]
  x1 <- strsplit(x = x1, split = "[[:blank:]]+")
  x1 <- do.call(what = rbind, args = x1)
  names <- x1[1, ]
  x1 <- x1[-1, , drop = FALSE]
  colnames(x1) <- names
  ind <- c("name", "parm")
  x1 <- data.frame(x1[, ind, drop = FALSE])
  ind <- c("parm")
  x1[, ind] <- mapply(FUN = as.numeric, x = x1[, ind])
  parm <- x1  
  print(parm)
  #### Answer
  list(parm = parm, nobs = nobs)
}
# ------------------------------------------------------------------------------ 


.naive.fit.symbols.dates <- function(file, period.out = NULL)
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
