library("shiny")
library("quantmod")
library("TTR")
library("shinydashboard")
library("data.table")
library("shiny")
library("tasi")

# Data Preparation --------------------------------------------------------

dt <- as.data.table(stock_indices)
sectors <- dt[, .(sectorName,companyNameAR)][, unique(sectorName)]
indicators <-   c("Voltality",
                  "SMA_30",
                  "SMA_5",
                  "Trend Detection Indicator",
                  "Bolinger Bands",
                  "Momentum"
                  )

# App UI ------------------------------------------------------------------

ui <- dashboardPage(
        
        dashboardHeader(title = "Saudi Stock Analyzer"), 
        
        dashboardSidebar(
            selectizeInput(
                inputId = "sector",
                label = "Select Industry",
                choices = sectors
                ), 
            selectizeInput(
                inputId = "comp",
                label = "Select Company",
                choices = NULL),
            br(),
            dateRangeInput(
                inputId = "date_selector",
                label = "Select Date Range",
                min = "2010-01-01",
                start = "2022-07-01",
                end = as.character(Sys.Date()),
                format = "yyyy-mm-dd"
                ),
       
            checkboxGroupInput(
                inputId = 'ttr',
                label = "Technical Indicators",
                inline = TRUE,
                choiceNames = indicators,
                choiceValues = 1:length(indicators)
                ),
            # parameter_tabs,
            
            actionButton(
                inputId = "execute", "Execute"
                )
            ),
        
        dashboardBody(
            box(
                width = 12,
                plotOutput("stock_analysis"),
                plotOutput("var_sensitivity"),
                plotOutput("return_distribution")
                )
            )
        )



# App Server --------------------------------------------------------------
server <- function(input, output, session) {
    data <- eventReactive(
        input$execute,
        {
            tasi::get_company_records(start_date = as.character(input$date_selector[1]), end_date = as.character(input$date_selector[2]), company_symbol = as.numeric(input$comp)) |>
            tasi::df_to_xts() |> 
            tasi::add_adj_price(input$comp)
            }
        )
    
    observeEvent(input$ttr, {
      req(input$trr)
      selection <- indicators[as.numeric(input$ttr)]
    })
    add_TDI <- function(){
      quantmod::add_TA(TTR::TDI(Cl(data()),n = 20),on = 1)
    }
    TAI <- reactive(
        {
            long <- min(nrow(data()),30)
            short <- min(nrow(data()),5)
            td_n <- min(nrow(data()),20) 
            volt <- min(nrow(data()),10)
            funs <- c(
                "add_Vo(n = 10)",
                "add_SMA(n = 30)",
                "add_SMA(n = 5)",
                "add_TDI()",
                "add_BBands(n = 10)",
                "add_Momentum()"
                )
        
            if (!is.null(input$ttr)){
                paste0(funs[as.numeric(input$ttr)],collapse = ";")
                } else { "" }
            })

    observe({
        sb_dt <- (
            dt[sectorName == input$sector, c("companyNameAR", "symbol")]
            )
        choices <- setNames(sb_dt[, symbol], sb_dt[,companyNameAR])
        updateSelectizeInput(
            session = session,
            inputId = "comp",
            label = paste("Select Company [",length(choices), "]"),
            choices = choices
            )
        })
    
    output$stock_analysis <- renderPlot({
        # generate bins based on input$bins from ui.R
        quantmod::chart_Series(data(),
                    TA = TAI()) 

    })
     output$var_sensitivity <- renderPlot({
     PerformanceAnalytics::chart.VaRSensitivity(quantmod::dailyReturn(Cl(data())))
     })
     output$return_distribution <- renderPlot({
     day_returns <- quantmod::dailyReturn(quantmod::Ad(data()))
         PerformanceAnalytics::chart.Histogram(day_returns)
     })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
