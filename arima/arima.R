# SUPPLEMENTARY CODE FOR THE MASTER PROJECT: 
# Extreme Risk Forecasting via Markov Regime Switching Models: Evidence from the Moroccan All Shares Index

# This script applies an AR(1) filter to MASI's logarithmic returns.

# AR(1) filter.
MASI.ts <- ts(MASI)
arima.fit <- Arima(MASI.ts, order = c(1, 0, 0))
arima.fit

# Residuals diagnostics.
residuals <- arima.fit$residuals
checkresiduals(arima.fit)