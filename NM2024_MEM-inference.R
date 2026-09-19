################################################################################
##
## File:    NM2024-mem-fit.R
##
## Purpose: MEM fit.
##  
## Created: 2021.01.20
##
## Version: 2024.02.02
##
## Remarks: 
##
################################################################################

#### Clean
remove(list = ls())

# PARAMETER ESTIMATION 

################################################################################
## Loading
################################################################################

#### Library
library(openxlsx)
#### Functions

source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-General-Functions.R")
source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-mem-Functions.R")
# source("~/Cipo/Teaching/Thesis/NucciMartina-2024/R/NM2024-General-Functions.R")
# source("~/Cipo/Teaching/Thesis/NucciMartina-2024/R/NM2023-mem-Functions.R")

.fit.print <- function(symbol, dates, fit, file)
{
  #### Prepare
  fit <- .mem.fit.print(fit = fit)
  x1 <- data.frame(symbol = symbol, in.first = dates[1], in.last = dates[2])
  #### Join
  fit <- c( 
    list(saved = Sys.time(), symbol.dates = x1),
    fit)
  #### 
  sink(file, append = TRUE)
  print(fit, quote = FALSE, row.names = FALSE)
  sink()
  cat("------------------------------------------------------------------------\n", 
      file = file, append = TRUE)
}
# ------------------------------------------------------------------------------


.inference.join <- function(inf.cum, inf.last)
{
  ####
  inf <- inf.last$table
  inf <- data.frame(coef = rownames(inf), inf, check.names = FALSE)
  #### 
  ind <- class(inf.cum) == "data.frame"
  if (ind)
  {
    tab <- data.frame(inf.cum[1,], inf)
    inf.cum <- list(symbols.dates = inf.cum, table = tab)
  }
  else
  {
    ind <- NROW(unique(inf.cum$table[, 1:5])) + 1
    tab <- data.frame(inf.cum$symbols.dates[ind,], inf)
    inf.cum$table <- rbind(inf.cum$table, tab)
  }
  ####
  inf.cum
}
# ------------------------------------------------------------------------------


################################################################################
## Inputs
################################################################################

#### Input

#dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\cripto_Yahoo"
dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\tassi_standard" 

#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2014-FittedModels-MEM11.txt"
#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2014-FittedModels-MEM21.txt"

#file.inference.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2014-FittedModels-MEM11.xlsx"
#file.inference.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2014-FittedModels-MEM21.xlsx"

#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2017-FittedModels-MEM11.txt"
#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2017-FittedModels-MEM21.txt"

#file.inference.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2017-FittedModels-MEM11.xlsx"
#file.inference.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-cripto2017-FittedModels-MEM21.xlsx"

#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-Fiat-FittedModels-MEM11.txt"
file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-Fiat-FittedModels-MEM21.txt"

#file.inference.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-Fiat-FittedModels-MEM11.xlsx"
file.inference.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\MEM-Fiat-FittedModels-MEM21.xlsx"


#############
#dir.data <- "~/Cipo/Teaching/Thesis/NucciMartina-2024/Data/Cripto Yahoo"
##file.out <- "~/Cipo/Teaching/Thesis/NucciMartina-2024/Out/MEM-FittedModels-01.txt"


# dir.data <- "~/Cipo/Teaching/Thesis/NucciMartina-2024/Data/Cripto Yahoo"
# dir.data <- "~/Cipo/Teaching/Thesis/NucciMartina-2024/Data/Tassi  standard"
# 
# file.out <- "~/Cipo/Teaching/Thesis/NucciMartina-2024/Out/MEM-Fiat-FittedModels-01.txt"
# file.inference.out <- "~/Cipo/Teaching/Thesis/NucciMartina-2024/Out/MEM-Fiat-FittedModels-MEM21.xlsx"

overnight <- FALSE

#### Estimation settings
## Estimation control settings
control <- list(
  algorithm = "NLOPT_LD_SLSQP", maxeval = 200, inner_maxeval = 300)
## List of symbols
symbols <- c(
  #'BTC_USD_Y','LTC_USD_Y') # cryptocurrencies, 2014 sample
  #'ADA_USD_Y','ETH_USD_Y','XRP_USD_Y','XLM_USD_Y','BCH_USD_Y','DOGE_USD_Y','ETC_USD_Y','TRX_USD_Y','LINK_USD_Y','BNB_USD_Y','USDT_USD_Y') # cryptocurrencies, 2017 sample
  'USD_AUD','USD_CAD','USD_EUR','USD_GBP','USD_JPY','USD_RUB','USD_BRL','USD_CHF','USD_KRW','USD_TRY','USD_TWD','USD_INR')

## List of dates for rolling estimation/forecasts
# Forecasting section
rolling.dates <- .rolling.dates(
  from = as.Date("2014-01-01"), to = as.Date("2024-02-01"), # start and end dates 
  span = 5, by = "12 months") # span: number of years used for estimation; by: length of the ex-post analysis period
# in: period used for estimation; out: ex-post forecast period


################################################################################
## Test area
################################################################################

# #### data
# file <- "~/Cipo/Volatility/Project/VMIDAS/data/Adjusted/XOM.txt"
# data <- .read.data(file = file, overnight = FALSE)
# parm <- .init.parm.mem.01(data = data)
# init <- .mem.init(parm = parm) 
# util <- list(pos = .pos.parm(parm = parm))
#  
# ####
# nrepl <- 1
# ####
# time <- Sys.time()
# for (i in 1 : nrepl)
# {
#   dflt1n <- maxLik::numericGradient(f = .mem.filter, t0 = parm, eps = 1e-06, 
#     data = data, init = init, util = util)
# }
# cat(Sys.time() - time, "\n")
# #### 
# time <- Sys.time()
# for (i in 1 : nrepl)
# {
#   dflt1 <- .mem.Dfilter(parm = parm, data = data, init = init, util = util)
# }
# cat(Sys.time() - time, "\n")
# ####
# cat( sum(abs(dflt1 - dflt1n)), "\n")
# 
# #### Filter
# flt1 <- .mem.filter(parm = parm, data = data, init = init, util = util)


################################################################################
## Make
################################################################################

#### Merge
symbols.dates <- data.frame(symbol = rep(symbols, each = NROW(rolling.dates)), 
                            rolling.dates) # joins each currency to the rolling-window table

#### To store estimated coefficients
inf.cum <- symbols.dates

#### Cycle
nsd <- NROW(symbols.dates)
for (i in 1 : nsd)
{
  #### Extract
  symbol <- symbols.dates[i, "symbol"]  # reads the currency from row i
  date.range <- c(symbols.dates[i, "in.first"], symbols.dates[i, "in.last"]) # reads the date range used for estimation (the in-sample period)
  #### Read data
  file <- file.path(dir.data, paste0(symbol, ".csv")) # builds the file path and reads the data
  # data <- .read.data(file = file, overnight = overnight, date.range = date.range)
  data <- .read.data(file = file, overnight = overnight, date.range = date.range)
  #### Starting values for parameters
  #parm <- .init.parm.mem.01(data = data)   ## beta[1], alpha[1], gamma[1]
  parm <- .init.parm.mem.02(data = data)   ## beta[1], alpha[1], alpha[2], gamma[1] 
  init <- .mem.init(parm = parm)   # sets the parameter starting values and initializes estimation
  #### Fit
  fit <- .mem.fit(parm = parm, data = data, init = init, control = control)
  #### Inference
  inf <- .mem.inference(fit = fit) # residuals and related quantities (focus on forecasts)
  #### Write
  inf.cum <- .inference.join(inf.cum = inf.cum, inf.last = inf)
  # .fit.print(symbol = symbol, date = date.range, fit = fit, file = file.out)
}

#### Write inference
write.xlsx(x = inf.cum$table, file = file.inference.out)
