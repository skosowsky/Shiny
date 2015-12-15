shinyUI(pageWithSidebar(
  headerPanel("Stocks"),
  
  sidebarPanel(
    wellPanel(
      p(strong("Stocks"))

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
    textInput("Ticker","Ticker:","^GSPC"),
    
    hr(),
   
   # submitButton(text="Submit"),

    checkboxInput(inputId = "log_y", label = "log y axis", value = FALSE)
  ),
  
  mainPanel(
    # plotOutput("Ticker_Plot"),
    sliderInput("Date_Range",
                label="Date Range",
                min=Sys.Date()-365,
                max=Sys.Date()-1,
                value=c(Sys.Date()-365,Sys.Date()-1),width = 1200),
    plotlyOutput("PLOTLY")
  )
))