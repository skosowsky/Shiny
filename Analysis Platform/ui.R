shinyUI(pageWithSidebar(
  headerPanel("Stocks"),
  
  sidebarPanel(
    wellPanel(
      p(strong("Stocks"))
   #  ,checkboxInput(inputId = c("stock_aapl"), label = c("Apple (AAPL)"),  
   #                 value = TRUE)
     
    ),
    
    selectInput(inputId = "chart_type",
                label = "Chart type",
                choices = c("Candlestick" = "candlesticks",
                            "Matchstick" = "matchsticks",
                            "Bar" = "bars",
                            "Line" = "line")
    ),
   selectInput(inputId = "Data_Source",
               label = "Data Source",
               choices = c("Yahoo" = "yahoo",
                           "Google" = "google")
   ),
    textInput("Ticker","Ticker:","CRM"),
    
    hr(),

    #dateRangeInput(inputId = "daterange", label = "Date range",
    #               start = Sys.Date() - 365, end = Sys.Date()),
    
    checkboxInput(inputId = "log_y", label = "log y axis", value = FALSE)
  ),
  
  mainPanel(
    plotOutput("Ticker_Plot"),
    sliderInput("Date_Range",
                label="Date Range",
                min=Sys.Date()-365,
                max=Sys.Date(),
                value=c(Sys.Date()-365,Sys.Date())),
    plotlyOutput("PLOTLY"),
    textOutput("drange")
   
  )
))