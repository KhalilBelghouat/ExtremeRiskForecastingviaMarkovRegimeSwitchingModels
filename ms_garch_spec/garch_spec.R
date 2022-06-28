# SUPPLEMENTARY CODE FOR THE MASTER PROJECT: 
# Extreme Risk Forecasting via Markov Regime Switching Models: Evidence from the Moroccan All Shares Index

# This script estimates different specifications of the single regime GARCH(1, 1) model via the ML method.

# Parallelizing code via The DoParallel package.
numCores <- detectCores() - 1
registerDoParallel(numCores)

# Model specification.
model <- list("sGARCH", "eGARCH", "gjrGARCH", "tGARCH")

# Error distribution.
distribution <- list("norm", "std", "ged", "snorm", "sstd", "sged")

# Model fit.
system.time({

garch.fit <- foreach(i = 1:length(model)) %:%

foreach(j = 1:length(distribution), .packages = c("MSGARCH")) %dopar% {

garch.spec <- CreateSpec(variance.spec = list(model = unlist(model[i])),
                         distribution.spec = list(distribution = unlist(distribution[j])),
                         switch.spec = list(K = 1))

try(FitML(spec = garch.spec, data = residuals))

}

})

# Model summary.
garch.fit