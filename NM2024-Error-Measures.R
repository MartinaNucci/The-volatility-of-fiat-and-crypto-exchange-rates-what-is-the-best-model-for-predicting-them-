remove(list = ls())

#### Functions
library(openxlsx)
library(forecast)

source("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\script\\NM2024-General-Functions.R")

# pred_mem11 <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_mem(1,1)_for.xlsx", sheet = "pred", detectDates = TRUE)
# pred_mem21 <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_mem(2,1)_for.xlsx", sheet = "pred", detectDates = TRUE)
# pred_har <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_har_for.xlsx", sheet = "pred", detectDates = TRUE)
# pred_naive <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\cripto_naive_for.xlsx", sheet = "pred",detectDates = TRUE)
# 
# dir.data<- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\cripto_Yahoo"

pred_mem11 <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_mem(1,1)_for.xlsx", sheet = "pred", detectDates = TRUE)
pred_mem21 <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_mem(2,1)_for.xlsx", sheet = "pred", detectDates = TRUE)
pred_har <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_har_for.xlsx", sheet = "pred", detectDates = TRUE)
pred_naive <- read.xlsx("C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\tassi_std_naive_for.xlsx", sheet = "pred",detectDates = TRUE)

dir.data<- "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\dati\\tassi_standard"

overnight<-FALSE


pred<-c("mu1","mu2","mu3","mu4","mu5")

results <- data.frame()

### TO DO
#data1<-merge(x=data.frame(date=data$date,y=data$y),y=pred_mem$


#s<-c("USD_AUD", "USD_CAD", "USD_EUR", "USD_GBP", "USD_JPY", "USD_RUB", "USD_BRL", "USD_CHF", "USD_KRW","USD_TRY","USD_TWD")

#### Cycle
for (symbol in unique(pred_mem21$symbol))
#for (symbol in s)

{
  #### Extract
  date.range <- pred_mem21[pred_mem21$symbol ==symbol, ]
  date.range <- range(date.range$date)
  
  #### Read data
  file <- file.path(dir.data, paste0(symbol, ".csv")) 
  data <- .read.data(file = file, overnight = overnight, date.range = date.range)

  y<-data$y

  
  for (j in pred)
  {
    y1<-y
    mem21 <- pred_mem21[pred_mem21$symbol == symbol, j]
    mem11 <- pred_mem11[pred_mem11$symbol == symbol, j]
    har <- pred_har[pred_har$symbol == symbol, j]
    naive <- pred_naive[pred_naive$symbol == symbol, j]
    # cat("NROW(mem21)",NROW(mem21),"\n")
    # cat("sum(mem21<=0)",sum(mem21<=0),"\n")
    # cat("sum(is.na(mem21)",sum(is.na(mem21)),"\n")
    
    ind<-mem21>0 & mem11>0 & har>0 & naive>0 
    # print (sum(ind))
    # print (sum(!ind))
    # print (sum(is.na(ind)))
    mem21<-mem21[ind]
    mem11<-mem11[ind]
    har<-har[ind]
    naive<-naive[ind]
    y1<-y1[ind]
    
    # 
     # Error measures
      cat("---------------------------------------------------------------------",
         "\nError measures\n","symbol:",symbol," pred:",j,"\n")
     ErrorMeas <- data.frame(
       measure = "Volatility",
       model =c("MEM(2,1)","MEM(1,1)","HAR"),
       rbind(
         round(.ErrorMeasures(y = y1,   fit=mem21, naive=naive, h=which(pred==j)), digits = 5),
         round(.ErrorMeasures(y = y1,   fit=mem11, naive=naive, h=which(pred==j)), digits = 5),
         round(.ErrorMeasures(y = y1,   fit=har, naive=naive,h=which(pred==j)), digits = 5))
    )
    print(ErrorMeas)
    # 
    # results <- rbind(results, ErrorMeas)
    
    # Diebold--Mariano test
     # cat("---------------------------------------------------------------------",
     #  "\nDiebold Mariano\n","symbol:",symbol," pred:",j,"Measure: Volatility","\n")
     # h <- which(pred==j)
     # msg1<-"MEM(2,1) vs HAR->"
     # msg2<-"MEM(1,1) vs HAR->"
     # msg3<-"MEM(1,1) vs MEM(2,1)->"
     # 
     # x1<-.DieboldMariano(y = y1, f1 = mem21, f2 = har, h = h, loss = "SE", msg = msg1)
     # x1<-.DieboldMariano(y = y1, f1 = mem21, f2 = har, h = h, loss = "LLE", msg = msg1)
     # x1<-.DieboldMariano(y = y1, f1 = mem11, f2 = har, h = h, loss = "SE", msg = msg2)
     # x1<-.DieboldMariano(y = y1, f1 = mem11, f2 = har, h = h, loss = "LLE", msg = msg2)
     # x1<-.DieboldMariano(y = y1, f1 = mem11, f2 = mem21, h = h, loss = "SE", msg = msg3)
     # x1<-.DieboldMariano(y = y1, f1 = mem11, f2 = mem21, h = h, loss = "LLE", msg = msg3)

  }
}

#write.xlsx(results, file = "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\Error_Measures_cripto.xlsx", rowNames = FALSE)
#write.xlsx(results, file = "C:\\Users\\marti\\OneDrive\\Desktop\\tesi\\output\\Error_Measures_Fiat.xlsx", rowNames = FALSE)
