library("quantmod")
library("TTR")
library("data.table")
library("tasi")
library("ggplot2")
library("dplyr")
library("forcats")
library("scales")
library("tidyquant")
library("gdeltr2")
library("plotly")
library("treemapify")

data <- get_daily_market_stats()

data %>% slice_max(n = 50, order_by = market_cap) %>% 
 filter(symbol != 2222) %>% 
  mutate( companyNameAR = fct_reorder( companyNameAR, market_cap)) %>%
  ggplot(aes(area = market_cap, fill = sectorName,subgroup = sectorName )) + 
  geom_treemap() + 
  geom_treemap_text(aes(
        label = scales::number(market_cap,accuracy = 1, scale = 0.0000000001, suffix = glue::glue("B {symbol}" )))
        ) +
  guides(fill = guide_legend(title = "Sector Name")) + 
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 14, "Set3"))


  
habib_df <- get_company_records(start_date = "2015-01-01", end_date = as.character(Sys.Date()), company_symbol = 4013, tidy = TRUE)
habib <- get_company_records(start_date = "2015-01-01", end_date = as.character(Sys.Date()), company_symbol = 4013)
habib_df %>% filter(Date > '2021-01-01') %>%  
  ggplot(mapping = aes(x = Date, y = Close)) + 
  geom_candlestick(
    aes(open = Open, close = Close, high = High, low = Low)
  )
adj_habib <- add_adj_price(habib, symbol = 4013 )

