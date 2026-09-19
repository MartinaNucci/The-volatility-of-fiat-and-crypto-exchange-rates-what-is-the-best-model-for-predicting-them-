################################################################################
##
## File:    BA2023-har-fit.R
##
## Purpose: HAR fit.
##  
## Created: 2021.01.20
##
## Version: 2023.09.15
##
## Remarks: 
##
################################################################################

#### Clean
remove(list = ls())


################################################################################
## Loading
################################################################################

#### Library
library(openxlsx)
#### Functions


source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-General-Functions.R")
source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-har-Functions.R")


.fit.print <- function(symbol, dates, fit, file)
{
  #### Prepare
  fit <- .har.fit.print(fit = fit)
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


################################################################################
## Inputs

#### Input

#dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\cripto_Yahoo"
dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\tassi_standard" 

#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_har_fit.txt"
file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_har_fit.txt"

#dir.data  <- "~/Cipo/Volatility/Project/VMIDAS/data/Adjusted"
#file.out <- "~/Cipo/Teaching/Thesis/BartolozziAlessio-2023/Out/Naive-FittedModels-01.txt"
overnight <- FALSE

#### Estimation settings
## List of symbols

symbols <- c(
  #'BTC_USD_Y','LTC_USD_Y') # cryptocurrencies, 2014 sample
  #'ADA_USD_Y','ETH_USD_Y','XRP_USD_Y','XLM_USD_Y','BCH_USD_Y','DOGE_USD_Y','ETC_USD_Y','TRX_USD_Y','LINK_USD_Y','BNB_USD_Y','USDT_USD_Y') # cryptocurrencies, 2017 sample
  'USD_AED','USD_AUD','USD_CAD','USD_CNY','USD_EUR','USD_GBP','USD_JPY','USD_RUB','USD_BRL','USD_CHF','USD_KRW','USD_TRY','USD_TWD',"USD_INR") # standard exchange-rate directory

## List of dates for rolling estimation/forecasts
rolling.dates <- .rolling.dates(
  from = as.Date("2014-01-01"), to = as.Date("2024-02-01"), 
  span = 5, by = "12 months") 


################################################################################
## Make
################################################################################

#### Merge
symbols.dates <- data.frame(symbol = rep(symbols, each = NROW(rolling.dates)), 
  rolling.dates)

#### Cycle
nsd <- NROW(symbols.dates)
for (i in 1 : nsd)
{

  #### Extract
  symbol <- symbols.dates[i, "symbol"]
  date.range <- c(symbols.dates[i, "in.first"], symbols.dates[i, "in.last"]) 
  #### Read data
  file <- file.path(dir.data, paste0(symbol, ".csv"))
  data <- .read.data(file = file, overnight = overnight, date.range = date.range)
  data.har <- .har.regressors(data = data)
  #### Fit
  fit <- .har.fit(data=data.har)
  #### Inference
  inf <- .har.inference(fit = fit) 
  #### File
  .fit.print(symbol = symbol, date = date.range, fit = fit, file = file.out)
}

