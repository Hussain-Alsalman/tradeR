## Scanner 
library("quantmod")
library("lubridate")
library("PerformanceAnalytics")
library("magrittr")
library("dplyr")
library("ggplot2")
library("gganimate")
library("tidyquant")
library("tasi")
library("data.table")

# Setting up local data
s_i <- stock_indices

#Extracting all companies from sector 
selected_companies<- setDT(s_i) [sectorName == "Banks",symbol]

#Setting up the 5 years period to date
cur_date <- as.character(Sys.Date()) 
start_d <- (date(cur_date)- years(5))
end_d <- cur_date

#Fetching data from the Stock market | Note: this usually takes a long time
portfolio <- tasi::getSymbols(start_date = start_d,
                              end_date = end_d,
                              symbol_vector = selected_companies,
                              tidy = TRUE)

#Converting Portfolio into Data.Table format
portfolio <- as.data.table(portfolio)
setkey(s_i, symbol)
setkey(portfolio, symbol)

#Statistical Analysis

portfolio[,lapply(.SD,function(x){sum(diff(log(x)))}), by = symbol, .SDcols =slct_cols]
portfolio[,as.list(summary(.SD)), by = symbol, .SDcols =slct_cols]


setorder(portfolio, Date)
setkey(portfolio, symbol)
port_f<- portfolio[s_i, nomatch = 0]


dff_n <- function(x,lag = 1){
  n <- length(x)
  return(
      c(
        rep(NA,lag),
        diff(
          log(x),lag = 1)))
}


cum_rt <- function(x){ 
  val <- dff_n(x)
  val <- replace(val, is.na(val), 0)
  cumsum(val)
}
port_f[, `:=`(cum_ret = cum_rt(Adjusted)), by= symbol]
port_f[, `:=`(cum_ret = cum_rt(Adjusted), product = (companyName |> grepl(pattern ="Cement",useBytes = FALSE) |> ifelse("cement", "noncement"))) , by= symbol]

#port_f[, `:=`(time = 1:.N), by = symbol]
port_f |>
  ggplot(aes(x = Date, y = cum_ret)) +
  geom_line(aes (color = companyName,group = symbol)) +
  geom_smooth() +
  theme_bw()


port_f[, lapply(.SD, unique), .SDcol = c("symbol", "companyName")]
 
 # Findings 
 ## Energy Sector: 
 ### - Aldrees Petroleum (Good Steady Returns)
 ### - Saudi Aramco 
 ## Cement Sector: 
 ### - Eastern Proviance Cement (Good Steady Returns)
 ### - City Cement Co
 
## Banks Sector:
### - 1120 Al Rajhi
### - 
 ## Capital Goods Sector: 
 ### - Astra Industrial Group (Good Steady Returns)
 
 

