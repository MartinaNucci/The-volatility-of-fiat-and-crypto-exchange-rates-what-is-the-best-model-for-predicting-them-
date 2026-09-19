################################################################################
##
## File:    BA2023-mem-Functions.R
##
## Purpose: MEM functions.
##  
## Created: 2021.01.14
##
## Version: 2023.03.20
##
## Remarks: 
##
################################################################################


################################################################################
## Control
################################################################################

.control.match <- 
function(x, xDef)
{
  ## FUNCTION:

  #### Clean x
  x <- x[names(x) %in% names(xDef)]
  #### Plug x into xDef
  xDef[names(x)] <- x
  #### Answer
  xDef
}
# ------------------------------------------------------------------------------


################################################################################
## Utilities
################################################################################

.parm.find <- function(parm, pattern)
{
  which( substr(names(parm), 1, nchar(pattern)) == pattern )
}
# ------------------------------------------------------------------------------


.pos.parm <- 
function(parm) 
{
  #### Settings
  parm <- names(parm) 
  #### Answer
  list(
    omega.x = grep(x = parm, pattern = "omega.x", fixed = TRUE), 
    beta.x  = grep(x = parm, pattern = "beta.x",  fixed = TRUE), 
    alpha.x = grep(x = parm, pattern = "alpha.x", fixed = TRUE), 
    gamma.x = grep(x = parm, pattern = "gamma.x", fixed = TRUE) )  
}
# ------------------------------------------------------------------------------


.parm.list <- 
function(parm, pos) 
{
  #### Answer
  list(
    omega.x = parm[pos$omega.x], 
    beta.x  = parm[pos$beta.x], 
    alpha.x = parm[pos$alpha.x], 
    gamma.x = parm[pos$gamma.x] )  
}
# ------------------------------------------------------------------------------


.omega.x <- 
function(mean, beta.x, alpha.x, gamma.x) 
{
  mean * (1 - sum( beta.x, alpha.x, 0.5 * gamma.x ) )
}
# ------------------------------------------------------------------------------


.data.cut <- 
function(data.f, dates) 
{
  ind <- which( dates[1] <= data.f$date & data.f$date <= dates[2] )
  list(date = data.f$date[ind], y = data.f$y[ind], ym = data.f$ym[ind] )
}
# ------------------------------------------------------------------------------


################################################################################
## Init
################################################################################

.init.parm.mem.01 <- 
function(data) 
{
  mean <- mean(data$y)
  beta.x  <- 0.6
  alpha.x <- 0.1
  gamma.x <- 0.0
  c(omega.x = mean * (1 - beta.x - alpha.x - gamma.x / 2),
    beta.x = beta.x, alpha.x = alpha.x, gamma.x = gamma.x)
}
# ------------------------------------------------------------------------------


.init.parm.mem.02 <- 
function(data) 
{
  mean <- mean(data$y)
  beta.x   <- 0.6
  alpha.x1 <- 0.1
  alpha.x2 <- 0.0
  gamma.x  <- 0.0
  c(omega.x = mean * (1 - beta.x - alpha.x1 - alpha.x2 - gamma.x / 2),
    beta.x = beta.x, alpha.x1 = alpha.x1, alpha.x2 = alpha.x2, 
    gamma.x = gamma.x)
}
# ------------------------------------------------------------------------------


.init.parm.mem <- 
function(data, parm) 
{
  ind <- .parm.find(parm = parm, pattern = "alpha.x")
  if ( NROW(ind) == 1 )
  {
    .init.parm.mem.01(data = data)
  }
  else 
  {
    .init.parm.mem.02(data = data)
  }
}
# ------------------------------------------------------------------------------


################################################################################
## MEM
################################################################################

.mem.init <- 
function(parm) 
{
  ## FUNCTION:
  
  #### Extract parameters
  ## Pos
  pos <- .pos.parm(parm)
  ## Parameters
  omega.x  <- parm[pos$omega.x]
  beta.x  <- parm[pos$beta.x]
  alpha.x <- parm[pos$alpha.x]
  gamma.x <- parm[pos$gamma.x]
  ## Sizes
  np  <- NROW(parm)
  nbeta.x  <- NROW(beta.x)
  nalpha.x <- NROW(alpha.x)
  ngamma.x <- NROW(gamma.x)
  ## Unconditional mean
  mu <- omega.x / (1 - sum(beta.x, alpha.x, gamma.x/2))
  
  #### Init
  xi   <- as.numeric(mu)
  xxi  <- rep(xi, length.out = nalpha.x)
  xxim <- rep(0.5 * xi, length.out = ngamma.x)
  xi.d   <- matrix(0, np, nbeta.x)
  xxi.d  <- matrix(0, np, nalpha.x)
  xxim.d <- 0.5 * xxi.d[, 1:ngamma.x, drop = FALSE]

  #### Answer
  list(
    xi = xi, xxi = xxi, xxim = xxim, 
    xi.d = xi.d, xxi.d = xxi.d, xxim.d = xxim.d)
}
# ------------------------------------------------------------------------------


.mem.filter <- 
function(parm, data, init, util)
{
  ## FUNCTION:
  
  #### Extract data
  y  <- data$y
  ym <- data$ym

  #### Extract init
  xiv     <- init$xi
  xxiv    <- init$xxi
  xximv   <- init$xxim

  #### Extract parameters
  ## Pos
  pos <- util$pos
  ## Parameters
  omega.x <- as.numeric(parm[pos$omega.x])
  beta.x  <- as.numeric(parm[pos$beta.x] )
  alpha.x <- as.numeric(parm[pos$alpha.x])
  gamma.x <- as.numeric(parm[pos$gamma.x])
  theta.x <- c(omega.x, beta.x, alpha.x, gamma.x) 
  ## Sizes
  np  <- NROW(parm)
  i.beta.x  <- 1 : NROW(beta.x)
  i.alpha.x <- 1 : NROW(alpha.x)
  i.gamma.x <- 1 : NROW(gamma.x)

  #### Additional
  nobs <- NROW(y)
  
  #### Initialize
  xi.store  <- rep.int(NA, nobs)
  
  #### Cycle
  for ( t in 1 : nobs )
  { 
    #### Aux
    x2 <- c(1, xiv, xxiv, xximv)
    
    #### xi[t]    
    xi <- sum(theta.x * x2) 

    #### Store
    xi.store[t] <- xi

    #### Update
    xxi  <- y[t]
    xxim <- ym[t]

    #### Update u0, u0m, du0, du0m
    xiv   <- c(xi, xiv)[i.beta.x]
    xxiv  <- c(xxi, xxiv)[i.alpha.x]
    xximv <- c(xxim, xximv)[i.gamma.x]
  }
  
  #### Answer
  data.frame(mu = xi.store)
  # xi.store
}
# ------------------------------------------------------------------------------


.mem.Dfilter <- 
function(parm, data, init, util)
{
  ## FUNCTION:
  
  #### Extract data
  y  <- data$y
  ym <- data$ym
  bs <- data$bs

  #### Extract init
  xiv     <- init$xi
  xxiv    <- init$xxi
  xximv   <- init$xxim
  xiv.d   <- init$xi.d
  xxiv.d  <- init$xxi.d
  xximv.d <- init$xxim.d
  
  #### Extract parameters
  ## Pos
  pos <- util$pos
  ## Parameters
  omega.x <- parm[pos$omega.x]
  beta.x  <- parm[pos$beta.x]
  alpha.x <- parm[pos$alpha.x]
  gamma.x <- parm[pos$gamma.x]
  theta.x <- c(omega.x, beta.x, alpha.x, gamma.x) 
  ## Sizes
  np <- NROW(parm)
  i.beta.x  <- 1 : NROW(beta.x)
  i.alpha.x <- 1 : NROW(alpha.x)
  i.gamma.x <- 1 : NROW(gamma.x)

  #### Additional
  nobs <- NROW(y)
  
  #### Initialize
  xi.store    <- rep.int(NA, nobs)
  xi.d.store  <- matrix(NA, nobs, np)
  xxi.d  <- 0 * parm
  xxim.d <- 0 * parm

  #### Cycle
  for ( t in 1 : nobs )
  { 
    #### Aux
    x2  <- c(1, xiv, xxiv, xximv)
    dx2 <- cbind(0, xiv.d, xxiv.d, xximv.d)
    
    #### xi[t]
    xi <- sum(x2 * theta.x) 
    #### d(xi[t])/d(theta1, theta2)
    xi.d <- as.numeric(dx2 %*% theta.x) + x2

    #### Store
    xi.store[t] <- xi
    xi.d.store[t, ] <- xi.d

    #### Update
    xxi   <- y[t] 
    xxim  <- ym[t] 
    xxi.d  <- 0 * parm
    xxim.d <- 0 * parm

    #### Update 
    xiv   <- c(xi, xiv)[i.beta.x]
    xxiv  <- c(xxi, xxiv)[i.alpha.x]
    xximv <- c(xxim, xximv)[i.gamma.x]
    xiv.d   <- cbind(xi.d,   xiv.d)[, i.beta.x, drop = FALSE]
    xxiv.d  <- cbind(xxi.d,  xxiv.d)[, i.alpha.x, drop = FALSE]
    xximv.d <- cbind(xxim.d, xximv.d)[, i.gamma.x, drop = FALSE]
  }
  
  ####
  mu.store   <- xi.store
  mu.d.store <- xi.d.store
  colnames(mu.d.store) <- names(parm)
  
  #### Answer
  list( flt = mu.store, Dflt = mu.d.store)
  # mu.d.store
}
# ------------------------------------------------------------------------------


.mem.pred <- 
function(t1, hor, parm, data, init, util)
{
  ## FUNCTION:
  
  #### Extract data
  y  <- data$y
  ym <- data$ym
  
  #### Extract init
  xiv     <- init$xi
  xxiv    <- init$xxi
  xximv   <- init$xxim
  
  #### Extract parameters
  ## Pos
  pos <- util$pos
  ## Parameters
  omega.x <- as.numeric(parm[pos$omega.x])
  beta.x  <- as.numeric(parm[pos$beta.x] )
  alpha.x <- as.numeric(parm[pos$alpha.x])
  gamma.x <- as.numeric(parm[pos$gamma.x])
  theta.x <- c(omega.x, beta.x, alpha.x, gamma.x) 
  ## Sizes
  np  <- NROW(parm)
  i.beta.x  <- 1 : NROW(beta.x)
  i.alpha.x <- 1 : NROW(alpha.x)
  i.gamma.x <- 1 : NROW(gamma.x)
  
  #### Additional
  nobs <- NROW(y)
  
  #### Initialize
  t1 <- t1[1]
  hor <- ifelse(hor[1] <= 1, 1, round(hor))
  xi.store  <- matrix(NA, nobs, hor)
  colnames(xi.store) <- paste0("mu", 1 : NCOL(xi.store))
  
  #### Cycle 1 : (t1+1-hor)
  ind <- if (t1 < nobs) {1: (t1 - hor)} else {1:nobs}
  for ( t in ind )
  { 
    #### Aux
    x2 <- c(1, xiv, xxiv, xximv)
    
    #### xi[t]    
    xi <- sum(theta.x * x2) 
    
    #### Store
    xi.store[t, 1] <- xi
    
    #### Update
    xxi  <- y[t]
    xxim <- ym[t]
    
    #### Update u0, u0m, du0, du0m
    xiv   <- c(xi, xiv)[i.beta.x]
    xxiv  <- c(xxi, xxiv)[i.alpha.x]
    xximv <- c(xxim, xximv)[i.gamma.x]
  }
  #### Cycle t1 : nobs
  ind  <- if (t1 < nobs) {(t1 + 1 - hor) : nobs} else {NULL}
  indh <- if (hor < 2) {NULL} else {2 : hor}
  for ( t in ind )
  { 
    #### Aux
    x2 <- c(1, xiv, xxiv, xximv)
    
    #### xi[t]    
    xi <- sum(theta.x * x2) 
    
    #### Store
    xi.store[t, 1] <- xi

    #### Copy from the last one
    xi1 <- xi
    xiv1  <- xiv
    xxiv1 <- xxiv
    xximv1 <- xximv
    
    ####
    for (h in indh)
    {  
      #### Update
      xxi1  <- xi1
      xxim1 <- 0.5 * xi1
      
      #### Update u0, u0m, du0, du0m
      xiv1   <- c(xi1, xiv1)[i.beta.x]
      xxiv1  <- c(xxi1, xxiv1)[i.alpha.x]
      xximv1 <- c(xxim1, xximv1)[i.gamma.x]

      #### Aux
      x2 <- c(1, xiv1, xxiv1, xximv1)
      
      #### xi[t]    
      xi1 <- sum(theta.x * x2) 
      
      #### Store
      xi.store[t, h] <- xi1
    }

    #### Update
    xxi  <- y[t]
    xxim <- ym[t]
    
    #### Update u0, u0m, du0, du0m
    xiv   <- c(xi, xiv)[i.beta.x]
    xxiv  <- c(xxi, xxiv)[i.alpha.x]
    xximv <- c(xxim, xximv)[i.gamma.x]
  }
  
  #### Answer
  data.frame(.pred.shift(pred = xi.store, t1 = t1, hor = hor))
  # xi.store
}
# ------------------------------------------------------------------------------


################################################################################
## Model fit
################################################################################

.nloptr.control <- function(control)
{
  #### Default
  controlDef <- list(
    check_derivatives = FALSE,
    check_derivatives_print = "errors",
    ftol_abs = 1e-8, ftol_rel = 1e-10, 
    xtol_abs = 1e-8, xtol_rel = 1e-10,
    algorithm = "NLOPT_LD_SLSQP", "NLOPT_LD_MMA", "NLOPT_LD_CCSAQ",
    print_level = 1, 
    maxeval = 400, 
    maxtime = 3600)

  #### Auxiliary
  fun1 <- function(x, xDef)
  {
    if ( (NROW(x) > 0) && !(x %in% xDef) )
    {
      x <- xDef[1]
    }
    x
  }
  
  #### Make
  control$algorithm <- fun1(x = control$algorithm[1], 
    xDef = controlDef$algorithm)
  controlDef$algorithm <- controlDef$algorithm[1]
  .control.match(x = control, xDef = controlDef)
} 
# -----------------------------------------------------------------------------


.mem.obj.grad <- function(parm, data, init, util)
{
  flt <- .mem.Dfilter(parm = parm, data = data, init = init, util = util)
  mu  <- flt$flt
  eps <- data$y / mu
  list(
    objective = -sum( log(eps) - eps + 1 ), 
    gradient  = -colSums( flt$Dflt * ((eps - 1) / mu) ))
}
# -----------------------------------------------------------------------------


.mem.fit <- 
function(parm, data, init, control)
{
  ## FUNCTION:

  #### Control
  if ( missing(control) ) control <- NULL
  control <- .nloptr.control(control = control)
  
  #### Trace
  time <- Sys.time()
  #### Utilities
  parm0 <- parm
  util <- list(pos = .pos.parm(parm))
  #### Fit
  fit <- nloptr::nloptr(x0 = parm0, eval_f = .mem.obj.grad,
    lb = NULL, ub = NULL, eval_g_ineq = NULL, eval_g_eq = NULL,
    opts = control,
    data = data, init = init, util = util)
  #### Trace
  time <- as.numeric(difftime(time1 = Sys.time(), time2 = time, units = "secs"))

  #### Answer
  list(
    model = list(start = parm0, data = data, init = init, util = util), 
    fit = c(list(method = "ml"), fit, time = time) )
}
# ------------------------------------------------------------------------------


.mem.inference <- 
function(fit) 
{
  ## FUNCTION:
  
  #### Extract
  data  <- fit$model$data
  init  <- fit$model$init
  util  <- fit$model$util
  nobs  <- NROW(data$y)
  parNames <- names(fit$model$start)
  parm0 <- fit$fit$x0            ## Starting
  parm  <- fit$fit$solution      ## Estimates
  names(parm) <- names(parm0) <- parNames
  np <- NROW(parm)

  #### Filter
  flt <- .mem.filter(parm = parm, data = data, init = init, util = util)
  #### Sigma
  eps <- data$y / flt[, "mu"]
  flt <- cbind(flt, eps = as.numeric(eps))
  u   <- eps - 1
  sigma <- sqrt( mean(u^2) )
  #### Standard errors
  vcov <- .mem.vcov(parm = parm, data = data, init = init, util = util) 
  se <- sqrt(diag(vcov))
  #### Loss function
  x1 <- .mem.obj.grad(parm = parm, data = data, init = init, util = util)
  obj  <- x1$obj
  grad <- x1$grad
  #### Fit measures
  ## Gamma log-likelihood
  phi <- 1 / sigma^2
  ll.gamma <- nobs * (phi * (log(phi) - 1) - lgamma(phi)) - sum(log(data$y)) - phi * obj
  ## IC
  ic <- .IC(loglik = ll.gamma, np = np, nobs = nobs)
  ## R^2
  R2 <- cor(data$y, flt[, "mu"])^2
  R2adj <- 1 - (1 - R2) * (nobs - 1) / (nobs - np)
  ## Join
  gof <- c(ic, R2 = R2, R2adj = R2adj)
    
  #### Table
  est <- c(parm, sigma = sigma)
  se  <- c(se, sigma = NA)
  grad <- c(grad, sigma = NA)
  table <- data.frame(
    est = est, se = se, zstat = est / se, grad = grad)
  rownames(table) <- names(est)
  
  #### Answer
  list(
    nobs = nobs, time = fit$fit$time, msg = fit$fit$message, 
      nIter = fit$fit$iter,
    fobj = obj, gof = gof, sigma = sigma,
    parm = parm,
    eps = c(mean = mean(eps), sd = sd(eps)), 
    vcov = vcov, table = table, flt = flt, 
    model = fit$model, fit = fit$fit)
}
# ------------------------------------------------------------------------------


.mem.vcov <- 
function(parm, data, init, util) 
{
  #### Filter
  flt <- .mem.Dfilter(parm = parm, data = data, 
    init = init, util = util)
  #### sigma^2
  u <- data$y / flt$flt - 1
  sigma2 <- mean(u^2)
  #### vcov
  vcov <- crossprod(flt$Dflt / flt$flt)
  vcov <- tryCatch(
    solve(vcov),
    error = function(cond) 
    {
      MASS::ginv(vcov)
    } 
  )
  vcov <- sigma2 * vcov
  rownames(vcov) <- colnames(vcov) <- names(parm)
  #### Answer
  vcov
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



################################################################################
## PLOT
################################################################################

################################################################################
## Manage output
################################################################################

.mem.fit.print <- function(fit)
{
  ####
  nobs  <- NROW(fit$model$data$y)
  mu.start <- fit$model$init$xi
  name  <- names(fit$model$start) 
  parm0 <- as.numeric(fit$fit$x0) 
  parm1 <- fit$fit$solution 
  list( 
    nobs = nobs,
    mu.start = mu.start, 
    parm = data.frame(name = name, parm0 = parm0, parm = parm1), 
    fit = data.frame(
      alg = c(fit$fit$options$algorithm), 
      ll = -c(fit$fit$objective), 
      niter = c(fit$fit$iterations) ) )
}
# ------------------------------------------------------------------------------ 


.mem.fit.read <- function(file, symbol, dates)
{
  #### Read
  x1 <- readLines(con = file)
  #### Select block by symbol and dates
  x2 <- .mem.fit.symbols.dates(file = file)
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
  x2 <- gsub(x = x2, pattern = " ", replacement = "", fixed = TRUE)
  nobs <- as.numeric(x2)
  x1 <- x1[(ind+3):NROW(x1)]
  #### mu.start
  pattern <- "$mu.start"
  ind <- which( x1 == pattern )
  x2 <- x1[ind+1]
  x2 <- gsub(x = x2, pattern = "[1]", replacement = "", fixed = TRUE)
  x2 <- strsplit(x = x2, split = "[[:blank:]]+")[[1]]
  x2 <- as.numeric(x2)
  mu.start <- x2[!is.na(x2)]
  x1 <- x1[(ind+3):NROW(x1)]
  #### fit
  pattern <- "$fit"
  ind <- which(x1 == pattern)
  parm <- x1[2:(ind-2)]
  fit <- x1[(ind+1):NROW(x1)]
  #### parm
  x1 <- parm
  x1 <- strsplit(x = x1, split = "[[:blank:]]+")
  x1 <- do.call(what = rbind, args = x1)
  names <- x1[1, ]
  x1 <- x1[-1, ]
  colnames(x1) <- names
  ind <- c("name", "parm0", "parm")
  x1 <- data.frame(x1[, ind])
  ind <- c("parm0", "parm")
  x1[, ind] <- mapply(FUN = as.numeric, x = x1[, ind])
  parm <- x1
  ####
  x1 <- fit
  x1 <- strsplit(x = x1, split = "[[:blank:]]+")
  x1 <- do.call(what = rbind, args = x1)
  names <- x1[1, ]
  x1 <- x1[-1, , drop = FALSE]
  colnames(x1) <- names
  ind <- c("alg", "ll", "niter")
  x1 <- data.frame(x1[, ind, drop = FALSE])
  ind <- c("ll", "niter")
  x1[, ind] <- mapply(FUN = as.numeric, x = x1[, ind])
  fit <- x1
  #### Answer
  list(fit = fit, parm = parm, nobs = nobs, mu.start = mu.start)  
}
# ------------------------------------------------------------------------------ 


.msmem.fit.read <- function(file, symbol, dates)
{
  #### Read
  x1 <- readLines(con = file)
  #### Select block by symbol and dates
  x2 <- .msmem.fit.symbols.dates(file = file)
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
  #### mu.start
  pattern <- "$mu.start"
  ind <- which( x1 == pattern )
  x2 <- x1[ind+1]
  x2 <- gsub(x = x2, pattern = "[[]1[]][[:blank:]]*", replacement = "", fixed = FALSE)
  x2 <- strsplit(x = x2, split = "[[:blank:]]+")[[1]]
  x2 <- as.numeric(x2)
  mu.start <- x2
  # x1 <- x1[(ind+3):NROW(x1)]
  #### fit/parm
  pattern <- "$parm"
  ind1 <- which(x1 == pattern)
  pattern <- "$fit"
  ind2 <- which(x1 == pattern)
  parm <- x1[(ind1+1):(ind2-2)]
  fit <- x1[(ind2+1):(ind2+3)]
  #### parm
  x1 <- parm
  x1 <- strsplit(x = x1, split = "[[:blank:]]+")
  x1 <- do.call(what = rbind, args = x1)
  names <- x1[1, ]
  x1 <- x1[-1, ]
  colnames(x1) <- names
  ind <- c("name", "parm0", "parm1", "parm")
  x1 <- data.frame(x1[, ind])
  ind <- c("parm0", "parm1", "parm")
  x1[, ind] <- mapply(FUN = as.numeric, x = x1[, ind])
  parm <- x1
  ####
  x1 <- fit
  x1 <- strsplit(x = x1, split = "[[:blank:]]+")
  x1 <- do.call(what = rbind, args = x1)
  names <- x1[1, ]
  x1 <- x1[-1, ]
  colnames(x1) <- names
  ind <- c("alg", "ll", "niter")
  x1 <- data.frame(x1[, ind])
  ind <- c("ll", "niter")
  x1[, ind] <- mapply(FUN = as.numeric, x = x1[, ind])
  fit <- x1
  #### Answer
  list(fit = fit, parm = parm, nobs = nobs, mu.start = mu.start)  
}
# ------------------------------------------------------------------------------ 


# .msmem.fit.symbols <- function(file)
# {
#   #### Read
#   x1 <- readLines(con = file)
#   #### Select
#   pattern <- "$nobs"
#   ind <- which(x1 == pattern)
#   x1 <- x1[ind-1]
#   #### Remove undesired elements
#   x1 <- gsub(x = x1, pattern = "[1] ", replacement = "", fixed = TRUE)
#   x1 <- gsub(x = x1, pattern = "\"", replacement = "", fixed = TRUE)
#   gsub(x = x1, pattern = '"', replacement = "", fixed = TRUE)
# }
# # ------------------------------------------------------------------------------ 


.mem.fit.symbols.dates <- function(file, period.out = NULL)
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


.mem.plot <- function(inf)
{
  leg  <- c(     "y",  "mu")
  col  <- c("grey70", "red")
  lwd  <- c(       1,     1)
  plot(x = inf$model$data$date, y = inf$model$data$y, type = "l", 
    xlab = "", ylab = "", main = "", col = col[1], lwd = lwd[1])
  lines(x = inf$model$data$date, y = inf$flt$mu, col = col[2], lwd = lwd[2])
  legend(x = "topleft", legend = leg, col = col, lwd = lwd, 
    fill = FALSE, border = "white", bty = "o")
}
# ------------------------------------------------------------------------------

