library("quantmod")
library("TTR")
library("data.table")
library("tasi")

#

# v -----------------------------------------------------------------------
stock_indices[, c("symbol","companyNameAR")]


### Company Valuation
## Market Cap
library("httr")
library("rvest")
library("dplyr")
comp_code <- 2020
url <- paste0("https://www.saudiexchange.sa/wps/portal/tadawul/market-participants/issuers/issuers-directory/company-details/!ut/p/z1/pZBLb4JAFIV_CwvWcxxQsLspWJiCJEjxMRszttGS8Fqghv76TrEbEks13t1Nvu_c3EMEWRNRylN2kE1WlTJX-0ZMtpEfOj5sGnizNwNs4s6dRcQpALLqAEodezQ1ESK0RgrwwOO5aSA2iLjL93hkgcXMX74sFWrTx3yYt_n4Yxj-81-JOOTV7lLVZ9PUTzp0NPJDno-5rvT3qqhl2SZtsasURNWoo6KfC5-6KncWBK4zpnge_wJDvfaBK8UNAj_NdMDA63WRpun6K9wnPONM074BGB7VBQ!!/p0/IZ7_NHLCH082KGET30A6DMCRNI2086=CZ6_NHLCH082KGET30A6DMCRNI2000=N/?tabID=profileTab&symbol=",comp_code,"&listingStatus=")
pg <- rvest::read_html(url)
pg  %>%
  html_element(css = ".stackable:nth-child(2) td:nth-child(2)") %>%
  html_text() %>%
  gsub(pattern = ",", replacement = "") %>%
  as.numeric()
## Saudi Aramco

library("tasi")
df <- rvest::read_html("https://www.saudiexchange.sa/Resources/Reports/DetailedDaily_en.html") %>%
  html_element(css = "tr:nth-child(4) .Table3 , tr:nth-child(4) .Table3 .portlet-right-padding-5, tr:nth-child(4) .Table3 .portlet-padding-5") %>%
  html_table()
colnames(df) <- df[1,]
df <- df[-1,]
df %>% mutate(Company = stringr::str_remove(`Company`,pattern = "[*]")) %>%
  left_join(stock_indices, by = c("Company" = "tradingNameEn")) %>%  colnames()
