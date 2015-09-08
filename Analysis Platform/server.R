
if (!require(quantmod)) {
  stop("This app requires the quantmod package. To install it, run 'install.packages(\"quantmod\")'.\n")
}
if (!require(plotly)) {
  stop("This app requires the plotly package")
}
# Download data for a stock if needed, and return the data





shinyServer(function(input, output) {
  
  require_symbol <- function(symbol, envir = parent.frame()) {
    if (is.null(envir[[symbol]])) {
      envir[[symbol]] <- getSymbols(symbol,src=input$Data_Source,auto.assign = FALSE,return.class="zoo")
    }
    
    envir[[symbol]]
  }
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Make a chart for a symbol, with the settings from the inputs
  make_chart <- function(symbol) {
    symbol_data <- require_symbol(symbol, symbol_env)
    
    chartSeries(symbol_data,
                name      = symbol,
                type      = input$chart_type,
                subset    = paste(input$Date_Range, collapse = "::"),
                log.scale = input$log_y,
                theme     = "white")
  }
  make_plotly<-function(symbol){
    symbol_data<-require_symbol(symbol,symbol_env)
    df<-as.data.frame(symbol_data)
   p<-plot_ly(y = df[,4], 
               x = time(symbol_data), 
               text = paste(time(symbol_data)))
   layout(p,xaxis=list(title="Date",
                       range=c(input$Date_Range),type="date",autorange=TRUE)
          ,yaxis=list(title="Price"))
  }
  output$drange<-renderText({as.Date(input$Date_Range)})
  
  output$Ticker_Plot<-renderPlot({make_chart(input$Ticker)})

    output$PLOTLY <- renderPlotly({ make_plotly(input$Ticker)
   
  })
 })