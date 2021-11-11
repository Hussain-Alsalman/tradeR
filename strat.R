### Importing Important Libraries

library("quantmod")
library("quantstrat")
library("TTR")
library("IKTrading")
library("tasi")

## Initializations
rm(list = ls(.blotter), envir = .blotter)

#Constants

initdate <- "2016-01-01"
from <- "2017-01-01" #start of backtest
to <- "2021-07-25" #end of backtest
currency("SAR") #Set up environment for currency to be used





symbols <- c(2222,2050,1010, 2350)
tasi::getSymbols("2015-02-01", "2021-07-28", symbols)
tradesize <-1000 #default trade size
initeq <- 100000 #default initial equity in our portfolio

symbols <-paste0("T",symbols)

strategy.st <- portfolio.st <- account.st <- "firststrat" #naming strategy, portfolio and account

#removes old portfolio and strategy from environment
rm.strat(portfolio.st)
rm.strat(strategy.st) 

#initialize portfolio, account, orders and strategy objects
initPortf(portfolio.st, symbols = symbols, initDate = initdate, currency = "SAR")

initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "SAR", initEq = initeq)


initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store=TRUE)



add.indicator(strategy = strategy.st,
              name = 'SMA',
              arguments = list(x = quote(Cl(mktdata)), n=200),
              label = 'SMA200')
add.indicator(strategy = strategy.st,
              name = 'SMA',
              arguments = list(x = quote(Cl(mktdata)), n=50),
              label = 'SMA50')
add.indicator(strategy = strategy.st,
              name = 'RSI',
              arguments = list(price = quote(Cl(mktdata)), n=3),
              label = 'RSI_3')




add.signal(strategy.st, name = 'sigComparison',
           arguments = list(columns=c("SMA50", "SMA200")),
           relationship = "gt",
           label = "longfilter")

add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns=c("SMA50", "SMA200")),
           relationship = "lt",
           lablel = "sigCrossover.sig")


add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "RSI_3", threshold = 20,
                            relationship = "lt", cross = FALSE),
           label = "longthreshold")



add.signal(strategy.st, name = "sigThreshold",
           arguments = list(column = "RSI_3", threshold = 80,
                            relationship = "gt", cross = TRUE),
           label = "thresholdexit")

add.signal(strategy.st, name = "sigFormula",
           arguments = list(formula = "longfilter & longthreshold",
                            cross = TRUE),
           label = "longentry")





add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "sigCrossover.sig", sigval = TRUE,
                          orderqty = "all", ordertype = "market",
                          orderside = "long", replace = FALSE,
                          prefer = "Open"),
         type = "exit")
  
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "thresholdexit", sigval = TRUE,
                          orderqty = "all", ordertype = "market",
                          orderside = "long", replace = FALSE,
                          prefer = "Open"),
         type = "exit")

add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "longentry", sigval = TRUE,
                          orderqty = 1, ordertype = "market",
                          orderside = "long", replace = FALSE,
                          prefer = "Open", osFUN = IKTrading::osMaxDollar,
                          tradeSize = tradesize, maxSize = tradesize),
         type = "enter")









out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)
updatePortf(portfolio.st)
daterange <- time(getPortfolio(portfolio.st)$summary)[-1]

updateAcct(account.st, daterange)
updateEndEq(account.st)


for(symbol in symbols){
  
  chart.Posn(Portfolio = portfolio.st, Symbol = symbol, 
             TA= c("add_SMA(n=50, col='blue')", "add_SMA(n=200, col='red')"))
}
