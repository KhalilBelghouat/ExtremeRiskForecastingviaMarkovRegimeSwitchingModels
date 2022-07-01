# SUPPLEMENTARY CODE FOR THE MASTER PROJECT: 
# Extreme Risk Forecasting via Markov Regime Switching Models: Evidence from the Moroccan All Shares Index

# This script computes the 1-step ahead GPD VaR for the single-regime GARCH specification and backtests the model.

# Model specifications.
sGARCH.sstd.spec <- CreateSpec(variance.spec = list(model = "sGARCH"),
                               distribution.spec = list(distribution = "sstd"),
                               switch.spec = list(K = 1)) 

models <- list(sGARCH.sstd.spec)
n.ots    <- 1275                # Number of out-of-sample evaluation.
n.its    <- 3830                # Fit sample size.
alpha    <- 0.05                # Risk level.
k.update <- 1                   # Estimation frequency.

# Initialization.
VaR             <- matrix(NA, nrow = n.ots, ncol = length(models))
y.ots           <- matrix(NA, nrow = n.ots, ncol = 1)
sGARCH.sstd.fit <- vector(mode = "list", length = length(models))

# Iterate over out-of-sample time.
for (i in 1:n.ots) {
  y.its    <- MASI[i:(n.its + i - 1)] 
  y.ots[i] <- MASI[n.its + i]         

  # Iterate over models.
  for (j in 1:length(models)) {
    
    # Update the model estimation.
    if (k.update == 1 || i %% k.update == 1) {
      cat("Model", j, "is reestimated\n")
      arima.fit <- Arima(ts(y.its), order = c(1, 0, 0))
      residuals <- arima.fit$residuals
      sGARCH.sstd.fit[[j]] <- FitML(spec = models[[j]], data = residuals,
                               ctr = list(do.se = FALSE))
      cvol <- Volatility(sGARCH.sstd.fit[[j]])
      standardized.residuals <- residuals/cvol
      gpd.fit <- gpdFit(-standardized.residuals, u = quantile(-standardized.residuals, 0.90))
      gpd.VaR <- gpdRiskMeasures(gpd.fit, prob = 1 - alpha)
    }

    # Calculate VaR 1-step ahead.
    VaR.1[i, j] <- forecast(arima.fit, h = 1)$mean - gpd.VaR$quantile*predict(sGARCH.sstd.fit[[j]], nahead = 1L)$vol
    
  }
}

# Backtesting VaR.
UC.pval <- CC.pval <- DQ.pval <- vector("double", length(models))

for (j in 1:length(models)) {
  test <- GAS::BacktestVaR(data  = y.ots,
                           VaR   = VaR.1[,j],
                           alpha = 0.05)

  UC.pval[j] <- test$LRuc[2]
  CC.pval[j] <- test$LRcc[2]
  DQ.pval[j] <- test$DQ$pvalue
}

names(UC.pval) <- names(CC.pval) <- names(DQ.pval) <- c("sGARCH.sstd")

# The unconditional coverage (CC) test.
print(UC.pval)

# The conditional coverage test.
print(CC.pval)

# The dynamic quantile test.
print(DQ.pval)