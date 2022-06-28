# SUPPLEMENTARY CODE FOR THE MASTER PROJECT: 
# Extreme Risk Forecasting via Markov Regime Switching Models: Evidence from the Moroccan All Shares Index

# This script estimates different specifications of the Markov regime-switching GARCH(1, 1) via the ML method.

# Parallelizing code via The DoParallel package.
numCores <- detectCores() - 1
registerDoParallel(numCores)

# Model specification.
model <- list("sGARCH", "eGARCH", "gjrGARCH", "tGARCH")

# Error distribution.
distribution <- list("norm", "std", "ged", "snorm", "sstd", "sged")

# Model fit.
system.time({

ms2.garch.fit <- foreach(i = 1:length(model)) %:%

foreach(j = 1:length(distribution), .packages = c("MSGARCH")) %dopar% {

ms2.garch.spec <- CreateSpec(variance.spec = list(model = unlist(model[i])),
                             distribution.spec = list(distribution = unlist(distribution[j])),
                             switch.spec = list(K = 2))

try(FitML(spec = ms2.garch.spec, data = residuals))

}

})

# Model summary.
ms2.garch.fit