remove(list = ls())

#### Functions
library(openxlsx)
library(forecast)

source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-General-Functions.R")

# pred_mem21 <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_mem(1,1)_for.xlsx", sheet = "pred", detectDates = TRUE)
# dir.data<- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\cripto_Yahoo"
# pdf(file = "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\Plots_cripto.pdf", width = 7, height = 5, paper = "a4", pointsize = 12)

pred_mem21 <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_mem(1,1)_for.xlsx", sheet = "pred", detectDates = TRUE)
dir.data<- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\tassi_standard"
pdf(file = "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\Plots_tassi_std.pdf", width = 7, height = 5, paper = "a4", pointsize = 12)

overnight<-FALSE



#### Cycle
for (symbol in unique(pred_mem21$symbol))
  
  
{
  #### Extract
  date.range <- pred_mem21[pred_mem21$symbol ==symbol, ]
  date.range <- range(date.range$date)
  
  #### Read data
  file <- file.path(dir.data, paste0(symbol, ".csv")) 
  data <- .read.data(file = file, overnight = overnight, date.range = date.range)
  
  y<-data$y
  cat(symbol, sum(data$y <= 0), "\n")
  # Plots
  plot(x = data$date, y = y, main = c("plots",symbol),    xlab = "", ylab = "", type = "l")
  Acf(x = y, lag.max = 100, type = "correlation", main = c("ACF",symbol) )
  Acf(x = y, lag.max = 100, type = "partial", main = c("PACF",symbol)) 
  
}
graphics.off()