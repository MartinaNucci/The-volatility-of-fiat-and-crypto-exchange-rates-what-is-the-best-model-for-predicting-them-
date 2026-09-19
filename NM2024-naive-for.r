################################################################################
##
## File:    BA2023-naive-flt.R
##
## Purpose: Naive forecasting.
##  
## Created: 2023.10.02
##
## Version: 2023.10.02
##
## Remarks: 
##
################################################################################

################################################################################
## Clear memory
################################################################################

rm(list=ls(all=TRUE))


################################################################################
## Load
################################################################################

#### Library
library(openxlsx)
#### Functions



source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-General-Functions.R")
source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-naive-Functions.R")

################################################################################
## Inputs
###############################################################################


#### Input

#dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\cripto_Yahoo"
dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\tassi_standard" 

#file.fit<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_naive_fit.txt"
file.fit <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_naive_fit.txt"

#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_naive_for.xlsx" 
file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_naive_for.xlsx"

#dir.data  <- "~/Cipo/Volatility/Project/VMIDAS/data/Adjusted"
#file.fit  <- "~/Cipo/Teaching/Thesis/BartolozziAlessio-2023/Out/Naive-FittedModels-01.txt"

period.out <- "12 months"
#file.out <- "~/Cipo/Teaching/Thesis/BartolozziAlessio-2023/Out/Naive-pred-01.xlsx"
overnight <- FALSE
hor <- 5


################################################################################
## Make
################################################################################

#### Symbols and Dates
symbols.dates <- .naive.fit.symbols.dates(file = file.fit, period.out = period.out)

#### Initialize
nsd <- NROW(symbols.dates)
parm.store <- pred.store <- vector(mode = "list", length = nsd) 

#### Cycle by symbol and period
for (i in 1 : nsd)
{
  #### Extract
  symbol <- symbols.dates[i, "symbol"]
  #### Read data used for model fit and forecasts
  file <- file.path(dir.data, paste0(symbol, ".csv"))
  date.range <- c(symbols.dates[i, "in.first"], symbols.dates[i, "out.last"]) 
  data <- .read.data(file = file, overnight = overnight, 
    date.range = date.range)
  #### Read fitted model
  date.range <- c(symbols.dates[i, "in.first"], symbols.dates[i, "in.last"]) 
  fit <- .naive.fit.read(file = file.fit, symbol = symbol, 
    dates = date.range)
    
  #### Flt (fit + forecasts periods)
  parm <- setNames(object = fit$parm$parm, nm = fit$parm$name)
  #flt <- .har.filter(parm = parm, data = data)
  flt <- data.frame(date = data$date, 
    .naive.filter(parm = parm, data = data))
  
  #### Forecasts
  # flt <- data.frame(date = data$date,
  #   .mem.filter(parm = parm, data = data, init = init, util = util))
  t1 <- .pred.t1(dates = data$date, date = symbols.dates[i, "out.first"])
  pred <- .naive.pred(t1 = t1, hor = hor, parm = parm, data = data)
  pred <- data.frame(date = data$date, pred )
  #### Forecasts only
  date.range <- c(symbols.dates[i, "out.first"], symbols.dates[i, "out.last"])
  ind <- date.range[1] <= pred$date & pred$date <= date.range[2]
  pred <- data.frame(symbols.dates[i,], pred[ind, , drop = FALSE])
  #### Store
  parm <- data.frame(symbols.dates[i,], fit$parm)
  parm.store[[i]] <- parm
  pred.store[[i]] <- pred
}

#### Join
parm.store <- do.call(what = rbind, args = parm.store)
pred.store <- do.call(what = rbind, args = pred.store)
x1 <- list(parm = parm.store, pred = pred.store)
openxlsx::write.xlsx(x = x1, file = file.out, overwrite = TRUE)

