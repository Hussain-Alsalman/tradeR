
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("tasi")
library("quantmod")
library("TTR")
library("shinydashboard")


choices <- stock_indices$symbol
names(choices) <- tasi::stock_indices$companyNameAR

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Saudi Stock Analyzer"), 
    dashboardSidebar(selectizeInput(inputId = "comp",
                                    label = "Select Company",
                                    choices= choices
    ), 
    dateRangeInput(
        inputId = "date_selector",
        label = "Select Date Range",
        min = "2010-01-01",
        start = "2021-01-01",
        end = as.character(Sys.Date())
    ),
    
    checkboxGroupInput(
        inputId = 'ttr',
        label = "Technical Indicators",
        inline = TRUE,
        choiceNames = c("Voltality",
                        "SMA 30",
                        "SMA 5",
                        "Trend Detection Indicator",
                        "Bolinger Bands",
                        "Momentum"
        ),
        choiceValues = c(1:6)
    ),
    actionButton(inputId = "execute","Execute")),
    dashboardBody(
        box(width = 12,
            plotOutput("stock_analysis"),
            plotOutput("var_sensitivity"),
            plotOutput("return_distribution")
        )
    )

)



# Define server logic required to draw a histogram
server <- function(input, output) {
    data <- eventReactive(input$execute,{
        stock_data <- tasi::get_company_records(input$date_selector[1], end_date = input$date_selector[2], company_symbol = input$comp)
        stock_data_xts <- tasi::df_to_xts(stock_data)
        stock_data_xts <- tasi::add_adj_price(stock_data_xts, input$comp)
        stock_data_xts
    })
    TAI <- reactive({
        funs <- c(addVolatility(),
                  addSMA(n = 30),
                  addSMA(n = 5),
                  addTDI(),
                  addBBands(),
                  addMomentum())
        if (!is.null(input$ttr)) funs[as.numeric(input$ttr)]
        else "NULL"
    })
    output$stock_analysis <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        chartSeries(data(),
                    TA = TAI(),
                    theme = chartTheme("white"),type = "candlesticks") 

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
