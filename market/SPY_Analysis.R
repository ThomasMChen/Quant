#Thomas Chen

library(quantmod)
library(quantstrat)

#STRATEGY
# BUY:
#   50-Day SMA > 200-Day SMA & DVOO < 20
# SELL:
#   50-Day SMA < 200-Day SMA

#QUANTSTRAT Initial Variables
# Test Data Range
initdate = "1999-01-01"
from = "2000-01-01"
to = "2010-10-23"
# Set system envr timezone:
Sys.setenv(TZ = "UTC")
# Set currency
currency("USD")
# Obtain financial Data
getSymbols("SPY", from = "2000-01-01", to = "2010-10-23", src = "yahoo", adjust = TRUE)
# Treat as basic equity
stock("SPY", currency = "USD", multiplier = 1)

#RETURN SPACE
tradesize <- 100000
initeq <- 100000

#STRATEGY HIEARCHY (Account <- Portfolio <- Strategy)
rm.strat(strategy.st) #remove existing strategy, prior to running
strategy.st <- portfolio.st <- account.st <- "firststrat"

# Initialize Portfolio 
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")
# Initialize Account 
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)
# Initialize Orders
initOrders(portfolio.st, initDate = initdate)
# Initialize Strategy
strategy(strategy.st, store = TRUE)

#INDICATORS
# # 200-Day SMA
# spy_sma = SMA(x = Cl(SPY), n = 200)
# # RSI 3-day Lookback
# spy_rsi = RSI(price = Cl(SPY), n = 3)
# RSI 3 to 4 day average 

# RSI_avg <- function(price, n1, n2) {
#   rsi_1 <- RSI(price = price, n = n1)
#   rsi_2 <- RSI(price = price, n = n2)
#   RSI_avg <- (rsi_1 +rsi_2)/2
#   
#   # output
#   colnames(RSI_avg) <- "RSI_avg"
#   return(RSI_avg)
# }

# DVO 2-Day Avg, 125 Day Lookback
DVO <- function(HLC, navg = 2, percentlookback = 126){
  # Ratio Closing Price to Avg of Hi/Lo
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  # Smooth out ratio with SMA
  avgratio <- SMA(ratio, n = navg)
  # Convert ratio to a 0-100 value using runPercentRank
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# #PLOT - INDICATORS
# quartz()
# plot(Cl(SPY))
# # Overlay 200-Day SMA
# lines(SMA(Cl(SPY), n=200), col="red")
# # RSI 2 
# quartz()
# plot(RSI(Cl(SPY), n = 2))

#ADD INDICATORS (To Strategy)
# 200 Day Simple Moving Average
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 200),
              label = "SMA200")

# 50 Day Simple Moving Average
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 50),
              label = "SMA50")

# DVO
add.indicator(strategy = strategy.st,
              name = "DVO",
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# # 3 Day RSI
# add.indicator(strategy = strategy.st,
#               name = "RSI",
#               arguments = list(price = quote(Cl(mktdata)), n = 3),
#               label = "RSI_3")
# 
# # 3-4 Day RSI Average
# add.indicator(strategy = strategy.st, 
#               name = "RSI_avg",
#               arguments = list(price = quote(HLC(mktdata)), n1 = 3, n2 = 4),
#               label = "RSI_3_4")
# DVO

#SIGNALS
# Spans Range When SMA50 < SMA200
add.signal(strategy.st, 
           name = "sigComparison",
           arguments = list(columns = c("SMA50", "SMA200"), relationship = "lt"),
           label = "filterexit")
# Hits When SMA50 > SMA200
add.signal(strategy.st,
           name = "sigCrossover",
           argument = list(columns = c("SMA50", "SMA200"), relationship = "gt"),
           label = "longfilter")

#Specifies that DVO_2_126 must be <20, longthreshold
#Comparison Signal (Range for undervalued price)
add.signal(strategy.st, name="sigThreshold",
            arguments = list(column ="DVO_2_126",
                             threshold = 20,
                             relationship = "lt",
                             cross = FALSE),
            label = "longthreshold")

#Specifies that DVO_2_126 must cross 80 
#Crossover Signal (Moment breaches undervalued price) 
add.signal(strategy.st,
           name = "sigThreshold",
           arguments = list(column = "DVO_2_126",
                            threshold = 80,
                            cross = TRUE,
                            relationship = "gt"),
           label = "thresholdfilter")

#Signal Formula
add.signal(strategy.st, 
           name = "sigFormula",
           arguments = list(formula = "longfilter & longthreshold",
                            cross= TRUE),
           label = "longentry")

#RULES
# Exit Rule - filterexit when SMA50 dips below SMA200
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "filterexit", 
                          sigval = TRUE, 
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE, 
                          prefer = "Open"),
         type = "exit")

#Enter Rule - longentry when SMA50 greater than SMA200 David Varadi < 20
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry",
                          sigval = TRUE,
                          orderqty = 1,
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "enter")

#Order Sizing Function - max dollar function
# add.rule(strategy.st, name = "ruleSignal",
#          arguments = list(sigcol = "longentry",
#                          sigval = TRUE,
#                          ordertype = "market",
#                          orderside = "long",
#                          replace = FALSE,
#                          prefer = "Open",
#                          osFUN = osMaxPos(
#                           data = quote(HLC(mktdata)),
#                           timestamp = ""),
#          type = "exit"))

#TEST INDICATORS
# test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY), price=Cl(SPY))
# test_subset <- test["2013-09-01/2013-09-05"]
# head(test_subset, n = 3)
# tail(test_subset, n = 3)
# test_init <- applyIndicators(strategy.st, mktdata=OHLC(SPY))
# test <- applySignals(strategy = strategy.st, mktdata = test_init)

#APPLY STRATEGY & UPDATE
test_init <- applyIndicators(strategy.st, mktdata=(OHLC(SPY)))
applySignals(strategy.st, mktdata = test_init)
applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

# Update Portfolio
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

# Update Account
updateAcct(account.st, daterange)
updateEndEq(account.st)

#TRADE STATISTICS
tstats <- tradeStats(Portfolios = portfolio.st)
tstats$Profit.Factor
tstats$Percent.Positive

# Plot Performance
# Add Indicators
sma50 <- SMA(x = Cl(SPY), n = 50)
sma200 <- SMA(x = Cl(SPY), n = 200)
dvo <- DVO(HLC = HLC(SPY), navg = 2, percentlookback = 126)

chart.Posn(Portfolio = portfolio.st, Symbol = "SPY")
add_TA(sma50, on = 1, col = "blue")
add_TA(sma200, on = 1, col = "red")
add_TA(dvo)

# P&L Analysis
# get instrument returns
instrets <- PortfReturns(portfolio.st)

# compute Sharpe ratio from returns
portpl <- .blotter$portfolio.firststrat$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)

# portPL <- .blotter$portfolio.firststrat$summary$Net.Trading.PL > head(portPL)
# SharpeRatio.annualized(portPL, geometric = FALSE)
# instrets <- PortfReturns(portfolio.st)
# head(instrets, n=3)
# tail(instrets, n=3)
