##############################################################################
##
## File:    NM2024-General-Functions.R
##
## Purpose: General functions.
##  
## Created: 2021.12.15
##
## Version: 2024.02.02
##
## Remarks: 
##
################################################################################


################################################################################
## Data
################################################################################

.read.data <- function(file, 
                       overnight = FALSE, date.range = NULL)
{ 
  #### Read
  data <- read.table(file = file, header = TRUE, sep = ",", quote = "",
                     na.strings = c(".","null"), check.names = FALSE, comment.char = "", 
                     stringsAsFactors = FALSE)
  
  data<- subset(data, !(Low > Open | Low == Open | Low == High))
  data.g<<-data
  
  #### Dates
  data$Date <- as.Date(x = data$Date)
  data <- data[order(data$Date), , drop = FALSE]
  #### Add
  data$retcc <- c(NA, diff(log(data$Close))) * (sqrt(252) * 100)
  data$retoc <- log(data$Close / data$Open) * (sqrt(252) * 100)
  data$retco <- c(NA, log(data$Open[-1] / data$Close[-NROW(data)]))
  
  #### Time series
  date <- data$Date
  y <- .garmanklass(data = data, sd = TRUE, currency = TRUE) * (sqrt(252) * 100)
  
  if (overnight[1])
  {
    ym <- y * (data$retcc < 0)
  }
  else
  {
    ym <- y * (data$retoc < 0)
  }
  
  #### Remove NA
  ind <- !is.na(y + ym)
  date <- date[ind]
  y <- y[ind]
  ym <- ym[ind]
  
  #### Dates
  if ( NROW(date.range) > 1 )
  {
    date.range <- range( date.range )
    ind <- date.range[1] <= date & date <= date.range[2]
    date <- date[ind]
    y <- y[ind]
    ym <- ym[ind]
  }
  
  #### Answer
  list(date = date, y = y, ym = ym)
}
# ------------------------------------------------------------------------------


.read.data.fiat <- function(file, 
                            overnight = FALSE, date.range = NULL)
{ 
  #### Read
  data <- read.table(file = file, header = TRUE, sep = ",", quote = "\"",
                     na.strings = ".", check.names = FALSE, comment.char = "", dec = ",",
                     stringsAsFactors = FALSE)
  colnames(data) <- c("Date", "Close", "Open", "High", "Low", "Volume", "VarPct")
  
  #### Dates
  data$Date <- as.Date(x = data$Date, format = "%d.%m.%Y")
  data <- data[order(data$Date), , drop = FALSE]
  #### Add
  data$retcc <- c(NA, diff(log(data$Close))) * (sqrt(252) * 100)
  data$retoc <- log(data$Close / data$Open) * (sqrt(252) * 100)
  data$retco <- c(NA, log(data$Open[-1] / data$Close[-NROW(data)]))
  
  #### Time series
  date <- data$Date
  y <- .garmanklass(data = data, sd = TRUE, currency = TRUE) * (sqrt(252) * 100)
  if (overnight[1])
  {
    ym <- y * (data$retcc < 0)
  }
  else
  {
    ym <- y * (data$retoc < 0)
  }
  
  #### Remove NA
  ind <- !is.na(y + ym)
  date <- date[ind]
  y <- y[ind]
  ym <- ym[ind]
  
  #### Dates
  if ( NROW(date.range) > 1 )
  {
    date.range <- range( date.range )
    ind <- date.range[1] <= date & date <= date.range[2]
    date <- date[ind]
    y <- y[ind]
    ym <- ym[ind]
  }
  
  #### Answer
  list(date = date, y = y, ym = ym)
}
# ------------------------------------------------------------------------------


.garmanklass <-
  function(data, 
           sd = TRUE, currency = FALSE)
  {
    #### Auxiliary
    currency <- currency[1]
    nobs  <- NROW(data)
    
    #### Intradaily
    ## Extract
    coef <- if ("Adjusted" %in% colnames(data) & !currency)
    { 
      data$Adjusted / data$Close
    }
    else 
    { 
      1
    }
    H1 <- log( data$High * coef )
    L1 <- log( data$Low * coef )
    O1 <- log( data$Open * coef )
    C1 <- log( data$Close * coef )
    u1 <- H1 - O1
    d1 <- L1 - O1
    c1 <- C1 - O1
    ## Values
    x <- 0.511 * (u1 - d1)^2 +
      (-0.019) * (c1 * (u1 + d1) - 2 * u1 * d1) +
      (-0.383) * c1^2
    # x <- 0.5 * (H1 - L1)^2 - (2 * log(2) - 1) * (C1 - O1)^2
    #### Overnight adjustment
    if ( !currency )
    {
      retco <- c(NA, log( data$Open[-1] / data$Close[-nobs] ) )
      retoc <- log( data$Close / data$Open )
      x1 <- sum( retco^2, na.rm = TRUE); x2 <- sum( retoc^2, na.rm = TRUE )  
      f  <- x1 / (x1 + x2)
      f[f < 0.01] <- 0.01; f[f > 0.99] <- 0.99
      a <- 0.12
      x <- a * retco^2 / f + ( (1 - a) / (1 - f) ) * x
    }
    
    #### Answer
    if ( sd ) { 1.034 * sqrt( x ) } else { x }
  }
# ------------------------------------------------------------------------------


################################################################################
## Rolling predictions
################################################################################

.rolling.dates <- 
  function(from, by, to, span) 
  {
    #### 
    fun <- function(from, by, span, to.max)
    {
      x0.in <- from 
      x1 <- as.numeric(format(x0.in, "%Y")) + span 
      x2 <- format(x0.in, "%m-%d") 
      x0.out <- as.Date(paste0(x1, "-", x2)) 
      x1.in <- x0.out - 1
      x1.out <- seq(from = x0.out, by = by, length.out = 2)[2] - 1
      x1.out <- min(to.max, x1.out)
      data.frame(in.first = x0.in, in.last = x1.in, out.first = x0.out, 
                 out.last = x1.out)
    }
    
    #### Prepare
    to.or <- to[1]
    to <- to.or
    to <- paste0( as.numeric(format(to, "%Y")) - span, "-", format(to, "%m-%d"))
    to <- as.Date(to)
    x1 <- seq(from = from[1], by = by[1], to = to[1]) 
    #### Make
    do.call(what = rbind, 
            args = mapply(FUN = fun, from = x1, 
                          MoreArgs = list(by = by, span = span, to.max = to.or), SIMPLIFY = FALSE))
  }
# ------------------------------------------------------------------------------


################################################################################
## sigma to distribution parameters
################################################################################

.gamma.sigma.2.par <-
  function(sigma2)
  {
    #### Extract
    phi <- 1 / sigma2
    #### Answer
    list(phi = phi, shape = phi, rate = phi)
  }
# ------------------------------------------------------------------------------


.lnorm.sigma.2.par <-
  function(sigma2)
  {
    #### Extract
    v <- log(sigma2 + 1)
    #### Answer
    list(v = v, meanlog = -0.5 * v, sdlog = sqrt(v))
  }
# ------------------------------------------------------------------------------


.betapr.sigma.2.par <-
  function(sigma2)
  {
    #### Extract
    beta <- 2 + 2 / sigma2
    alpha <- beta - 1
    #### Answer
    list(beta = beta, shape1 = alpha, shape2 = beta)
  }
# ------------------------------------------------------------------------------


.llogis.sigma.2.par <-
  function(sigma2)
  {
    #### Extract
    v  <- sigma2
    v1 <- v + 1
    #### Utilities
    fun <- function(b, v1)
    {
      tan(b) / b - v1
    }
    #### Compute b from sigma^2
    root <- uniroot(f = fun, v = v1,
                    lower = 0, upper = pi/2, f.lower = -v, f.upper = Inf, check.conv = TRUE, 
                    tol = .Machine$double.eps^0.5, maxiter = 1000, trace = 0)
    #### Retrieve beta from b
    b <- root$root
    beta <- pi / b
    alpha <- sin(b) / b
    #### Answer
    list(beta = beta, scale = alpha, shape = beta)
  }
# ------------------------------------------------------------------------------


.gpd.sigma.2.par <-
  function(sigma2)
  {
    #### Extract
    beta <- 0.5 * (1 / sigma2 + 1)
    xi   <- 1 - beta
    
    #### Answer
    list(xi = xi, mu = 0, beta = beta)
  }
# ------------------------------------------------------------------------------


.sigma.2.par <-
  function(sigma2)
  {
    #### Extract
    list(
      gamma  = .gamma.sigma.2.par(sigma2), 
      lnorm  = .lnorm.sigma.2.par(sigma2), 
      betapr = .betapr.sigma.2.par(sigma2), 
      llogis = .llogis.sigma.2.par(sigma2),
      gpd    = .gpd.sigma.2.par(sigma2) )
  }
# ------------------------------------------------------------------------------


################################################################################
## losses
################################################################################

.loss.mse <- function(y, fit)
{
  err <- y - fit
  err^2
}
# ------------------------------------------------------------------------------

.loss.qml <- function(y, fit)
{
  err <- y / fit
  err - 1 - log(err)
}
# ------------------------------------------------------------------------------


################################################################################
## Plot
################################################################################

.plot.all <- function(fit, file = "")
{
  #### Extract
  ind  <- grep(x = fit$mem$inference$parmName, pattern = "mu", fixed = TRUE)
  mup  <- fit$mem$inference$est[ind]
  eps  <- fit$mem$diagnostics$residuals
  xi   <- fit$mem$diagnostics$muFilter
  xDep <- fit$xDep
  cnames <- colnames(xDep)
  nc <- NCOL(xDep)
  
  ####
  if (file != "")
  {
    png(filename = file, width = 1600, height = 900, res = 150)
  }
  layout(mat = matrix(data = 1 : (5 * nc), nrow = nc, byrow = TRUE), 
         widths = 1, heights = 1, respect = FALSE)
  par(mar = c(2,3,3,0))
  ylim.ts <- c(0, max(xDep))
  ylim.xi <- range(xi)
  #### Cycle
  for ( ic in 1 : nc ) 
  {
    #### 1) Plot
    plot(x = fit$date,  y = xDep[, ic], type = "l", 
         ylim = ylim.ts, xlab = "", ylab = "", main = cnames[ic])  
    lines(x = fit$date, y = fit$sp$eta * mup[ic], col = "red")
    #### 2) Acf
    forecast::Acf(xDep[, ic], lag.max = 100, main = "ACF(y)")
    #### 3( Acf of residuals
    forecast::Acf(eps[, ic], lag.max = 100, main = "ACF(eps)")
    #### 4) Plot
    plot(x = fit$date, y = xi[, ic], type = "l", 
         ylim = ylim.xi, xlab = "", ylab = "", main = "xi")  
    abline(h = mup[ic], col = "red")
    #### 5) Hist
    .hist.all(eps = eps[, ic])
  }
  if (file != "")
  {
    graphics.off()
  }
}
# ------------------------------------------------------------------------------


.hist.all <-
  function(eps, main = "", legend = FALSE)
  {
    #### Parameters
    sigma2 <- var(eps)
    parm <- .sigma.2.par(sigma2 = sigma2)
    
    #### Plot settings  
    tab <- c(
      "Gamma",        "red",    1, 
      "Log-Normal",   "blue",   1,
      "Beta'",        "orange", 1,
      "Log-Logistic", "green",  1 ) 
    #   "Gen. Pareto",  "brown",  1 )
    tab <- matrix(data = tab, ncol = 3, byrow = TRUE)
    tab <- data.frame(dist = tab[, 1], col = tab[, 2], lty = as.numeric(tab[, 3]))
    
    #### Plot settings
    ne <- NROW(eps)
    rng <- range(eps)  
    x1 <- seq(from = 0.01, to = rng[2], length.out = 200)
    #### Pdf's
    pdf <- cbind(
      dgamma(x = x1, shape = parm$gamma$shape, rate = parm$gamma$rate),       ## Gamma
      dlnorm(x = x1, meanlog = parm$lnorm$meanlog, sdlog = parm$lnorm$sdlog), ## Log-Normal
      extraDistr::dbetapr(x = x1, shape1 = parm$betapr$shape1,                ## Beta' 
                          shape2 = parm$betapr$shape2),                     
      actuar::dllogis(x = x1, scale = parm$llogis$scale,                      ## Log-Logistic
                      shape = parm$llogis$shape))
    #   evir::dgpd(x = x1, xi = parm$gpd$xi, beta = parm$gpd$beta) )            ## Gen. Pareto
    #### Histogram
    hist <- hist(x = eps, breaks = 100, plot = FALSE)
    ylim <- c(0, max(pdf, hist$density))
    plot(hist, freq = FALSE, ylim = ylim, main = main)
    for (i in 1 : NCOL(pdf))
    { 
      lines(x = x1, y = pdf[, i], col = tab$col[i], lwd = 2)
    }
    #### Legend
    if (legend)
    {
      legend(x = "topright", legend = tab$dist, col = tab$col, lty = tab$lty, 
             fill = FALSE, border = "white")
    }
  }
# ------------------------------------------------------------------------------

.merge <- function(x)
{
  #### Merge
  x1 <- x[[1]] 
  for (i in 2 : ns)
  {
    x1 <- merge(x = x1, y = x[[i]], by = "date", all = TRUE)
  }
  #### Colnames
  if ( (NCOL(x)-1) == NROW(x))
  {
    colnames(x1)[-1] <- names(x)
  }
  #### Answer
  x1
}
# ------------------------------------------------------------------------------


################################################################################
## Forecast
################################################################################

.pred.t1 <- 
  function(dates, date)
  {
    #### Answer
    which(dates >= date[1])[1]
  }
# ------------------------------------------------------------------------------

.pred.shift <- 
  function(pred, t1, hor)
  {
    ## FUNCTION:
    
    ####
    nobs <- NROW(pred)
    ####
    for (h in 1 : hor)
    {
      ind1 <- (t1 + 1 - h) : (nobs + 1 - h)
      ind2 <- t1 : nobs
      pred[ind2, h] <- pred[ind1, h]
    }
    if (NCOL(pred) > 1)
    {
      pred[1 : (t1-1), 2 : NCOL(pred)] <- NA
    }
    
    #### Answer
    pred
  }
# ------------------------------------------------------------------------------


################################################################################
## Functions for prediction checking
################################v################################################
# 
# 
# # #------------------------------------------------------------
.ErrorMeasures <-
  function(y, fit, naive,h)
  {
    #### Errors
    nf <-NROW(fit)
    y1<-y[ (NROW(y) - nf + 1) : NROW(y) ]
    

    u  <- y1 - fit
    v  <- y1 / fit

    #### Error measures
    #ME   <- mean( u )
    MAE  <- mean( abs(u) )
    RMSE <- sqrt( mean( u^2 ) )
    #### Percentage error measures
  
    if ( all(y1 > 0) )
    {
      ur     <- u / y1
      #MPE    <- mean( ur )
      MAPE   <- mean( abs( ur ) )
      RMSPE  <- sqrt( mean( ur^2 ) )
      LLE    <- mean( v - 1 - log(v) )
    }
    else
    {
      MPE    <- NULL
      MAPE   <- NULL
      RMSPE  <- NULL
      LLE    <- NULL
    }

    #### Scaled error measures
    u1 <- y1 - naive
    ScMAE  <- MAE / mean( abs(u1) )
    ScRMSE <- RMSE / sqrt( mean( u1^2 ) )
    if ( all(y1 > 0) )
    {
      v1 <- y1 / naive
      #ScLLE <- LLE / mean( v1 - 1 - log(v1))
    }
    else
    {
      ScLLE <- NULL
    }

    ####
    c(MAE = MAE, RMSE = RMSE,MAPE = MAPE, RMSPE = RMSPE, LLE = LLE,
      ScMAE = ScMAE, ScRMSE = ScRMSE)
  }


#------------------------------------------------------------
.DieboldMariano <- 
  function(y, f1, f2, h, loss, msg = "")
  {
    #### Compute
    loss <- toupper(loss)
    if (loss == "LLE")
    {
      e1 <- y / f1
      e1 <- 1 - log(e1) + e1
      e2 <- y / f2
      e2 <- 1 - log(e2) + e2
    }
    else if (loss == "AE")
    {
      e1 <- abs(y - f1)
      e2 <- abs(y - f2)
    }
    else if (loss == "SE")
    {
      e1 <- (y - f1)^2
      e2 <- (y - f2)^2
    }
    else
    {
      stop("'loss' must be 'LLE' or 'AE' or 'SE'")
    }
    
    x1 <- dm.test(e1 = e1, e2 = e2, h = h, power = 1)
    #### Print
    if ( msg != "" )
    {
      cat(msg,
          "Horiz:", x1$parameter["Forecast horizon"],
          ", Loss fct:", loss,
          ", Stat (L1-L2):", x1$statistic, "\n")
    }
    ### Answer
    x1
  }

