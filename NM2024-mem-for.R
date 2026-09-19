################################################################################
##
## File:    BA2023-mem-fit.R
##
## Purpose: MEM forecasting.
##  
## Created: 2021.01.20
##
## Version: 2023.09.15
##
## Remarks: 
##
################################################################################

################################################################################
## Clear memory
################################################################################

#rm(list=ls(all=TRUE))

# FORECASTING

################################################################################
## Load
################################################################################

#### Library
library(openxlsx)
#### Functions


source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-General-Functions.R")
source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-mem-Functions.R")

################################################################################
## Inputs
################################################################################

#### Input
#dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\cripto_Yahoo" 
dir.data <- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\tassi_standard"

#file.fit<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_mem_fit(2,1).txt"
#file.fit<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_mem(1,1)_fit.txt"

#file.fit<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_mem(2,1)_fit.txt"
file.fit<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_mem(1,1)_fit.txt"

period.out <- "12 months" # period used for ex-post analysis

#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_mem(2,1)_for.xlsx"
#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_mem(1,1)_for.xlsx"

#file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_mem(2,1)_for.xlsx"
file.out<-"C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_mem(1,1)_for.xlsx"

overnight <- FALSE
hor <- 5 


################################################################################
## Make
################################################################################

#### Symbols and Dates
symbols.dates <- .mem.fit.symbols.dates(file = file.fit, period.out = period.out) #ricotruisce tab in out, si aggiunge "line" (riga di partenza)

#### Initialize
nsd <- NROW(symbols.dates)
# stores the estimated parameters
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
  fit <- .mem.fit.read(file = file.fit, symbol = symbol, 
    dates = date.range)
  mu.start <- fit$mu.start
  parm <- setNames(object = fit$parm$parm, nm = fit$parm$name)
  init <- .mem.init(parm = parm)
  util <- list(pos = .pos.parm(parm))
  #### Forecasts
  # flt <- data.frame(date = data$date,
  #   .mem.filter(parm = parm, data = data, init = init, util = util))
  t1 <- .pred.t1(dates = data$date, date = symbols.dates[i, "out.first"])
  pred <- .mem.pred(t1 = t1, hor = hor, 
      parm = parm, data = data, init = init, util = util)
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
pred.store  <- do.call(what = rbind, args = pred.store)
x1 <- list(parm = parm.store, pred = pred.store)
openxlsx::write.xlsx(x = x1, file = file.out, overwrite = TRUE)

