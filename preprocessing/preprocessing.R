# SUPPLEMENTARY CODE FOR THE MASTER PROJECT: 
# Extreme Risk Forecasting via Markov Regime Switching Models: Evidence from the Moroccan All Shares Index

# This script passes the MASI data through important preprocessing steps.

# Importing MASI data.
market.index.1 <- read.csv("1. Moroccan All Shares Historical Data.csv")
market.index.1[c("Price", "Open", "High", "Low")] <- lapply(market.index.1[c("Price", "Open", "High", "Low")], function(x) as.numeric(gsub(",", "", x)))
market.index.1 <- market.index.1 %>% map_df(rev)
market.index.2 <- read.csv("2. Moroccan All Shares Historical Data.csv")
market.index.2[c("Price", "Open", "High", "Low")] <- lapply(market.index.2[c("Price", "Open", "High", "Low")], function(x) as.numeric(gsub(",", "", x)))
market.index.2 <- market.index.2 %>% map_df(rev)
market.index.3 <- rbind(market.index.1, market.index.2)
market.index.Date <- mdy(market.index.3$Date) 
market.index <- zoo(market.index.3$Price, market.index.Date)

# Extracting the log-returns of the index and transform them into a zoo object.
log.returns <- diff(log(market.index.3$Price))*100
MASI.Date <- mdy(market.index.3$Date[-1])
MASI <- zoo(log.returns, MASI.Date)
