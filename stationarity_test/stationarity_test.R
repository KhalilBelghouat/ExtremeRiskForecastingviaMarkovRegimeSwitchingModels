# SUPPLEMENTARY CODE FOR THE MASTER PROJECT: 
# Extreme Risk Forecasting via Markov Regime Switching Models: Evidence from the Moroccan All Shares Index

# This script applies multiple stationarity tests to the MASI data.

# Augmented Dickey-Fuller test.
adf.test <- adf.test(log.returns)

# Kwiatkowski-Phillips-Schmidt-Shin test.
kpss.test <- kpss.test(log.returns)

# Phillips-Perron test.
pp.test <- pp.test(log.returns)