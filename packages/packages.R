# SUPPLEMENTARY CODE FOR THE MASTER PROJECT: 
# Extreme Risk Forecasting via Markov Regime Switching Models: Evidence from the Moroccan All Shares Index

# This script installs and loads all R packages required to reproduce the results.

# Installing R packages.
install.packages("zoo")
install.packages("GAS")
install.packages("aTSA")
install.packages("psych")
install.packages("tibble")
install.packages("readxl")
install.packages("MSGARCH")
install.packages("tseries")
install.packages("foreach")
install.packages("forecast")
install.packages("quantmod")
install.packages("parallel")
install.packages("tidyverse")
install.packages("fExtremes")
install.packages("lubridate")
install.packages("tsoutliers")
install.packages("doParallel")
install.packages("data.table")

# Loading the installed R packages.
library(zoo)
library(GAS)
library(aTSA)
library(psych)
library(tibble)
library(readxl)
library(MSGARCH)
library(tseries)
library(foreach)
library(quantmod)
library(forecast)
library(parallel)
library(lubridate)
library(tidyverse)
library(fExtremes)
library(tsoutliers)
library(doParallel)
library(data.table)

theme_set(theme_bw())