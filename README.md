# The-volatility-of-fiat-and-crypto-exchange-rates-what-is-the-best-model-for-predicting-them-#

The main objective is to compare the forecasting performance of the following models:
- Multiplicative Error Model MEM(1,1)
- Multiplicative Error Model MEM(2,1)
- Heterogeneous Autoregressive (HAR) model
- Naïve benchmark model
Volatility is measured using a range-based estimator derived from daily opening, closing, highest, and lowest prices.

The scripts are organised according to their purpose:

- `NM2024-General-Functions.R`  
  Contains general functions for data loading, volatility calculation, rolling windows, forecast evaluation, and Diebold–Mariano tests.

- `NM2024-mem-Functions.R`  
  Contains the functions required to specify, estimate, filter, and forecast the MEM models.

- `NM2024-har-Functions.R`  
  Contains the functions used to estimate and forecast the HAR model.

- `NM2024-naive-Functions.R`  
  Contains the functions for the naïve benchmark model.

- `NM2024-mem-fit.R`  
  Estimates the MEM model parameters over rolling windows.

- `NM2024-har-fit.R`  
  Estimates the HAR model parameters over rolling windows.

- `NM2024-naive-fit.R`  
  Fits the naïve benchmark model.

- `NM2024_MEM-inference.R`  
  Performs statistical inference for the MEM models and exports the estimated coefficients.

- `NM2024_HAR-inference.R`  
  Performs statistical inference for the HAR model and exports the estimated coefficients.

- `NM2024-mem-for.r`  
  Produces forecasts from the fitted MEM models.

- `NM2024-har-for.r`  
  Produces forecasts from the fitted HAR model.

- `NM2024-naive-for.r`  
  Produces forecasts from the naïve benchmark model.

- `NM2024-Error-Measures.R`  
  Calculates MAE, RMSE, MAPE, RMSPE, LLE, and scaled forecast-error measures. It also contains the code for Diebold–Mariano comparisons.

- `NM2024-Plots.R`  
  Generates time-series plots, autocorrelation functions, and partial autocorrelation functions.

- `tab_em_word.R`  
  Exports forecast-error tables from Excel to a Word document.

## Data
The analysis uses daily cryptocurrency and fiat exchange-rate data obtained from Yahoo Finance.
The datasets must contain the following variables:
- `Date`
- `Open`
- `High`
- `Low`
- `Close`

