
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
      envir[[symbol]] <- getSymbols(symbol,src=input$Data_Source,from=input$Date_Range[1],to=input$Date_Range[2],auto.assign = FALSE,return.class="zoo")
    }
    
    envir[[symbol]]
  }
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Make a chart for a symbol, with the settings from the inputs
  make_chart <- function(symbol) 
  {
    symbol_data <- require_symbol(symbol, symbol_env)
    
    chartSeries(symbol_data,
                name = symbol,
                type = input$chart_type,
                subset = paste(input$Date_Range, collapse = "::"),
                log.scale = input$log_y,
                theme = "white")
  }
  
  make_plotly<-function(symbol){
    symbol_data<-require_symbol(symbol,symbol_env)
    time_input <-between(as.Date(time(symbol_data),format = "%Y-%m-%d"),as.Date(input$Date_Range[1],format = "%Y-%m-%d"),as.Date(input$Date_Range[2],format = "%Y-%m-%d")) & coredata(symbol_data)[,5] != 0
    Open <- coredata(symbol_data)[time_input,1]
    Close <- coredata(symbol_data)[time_input,4]
    Low <- coredata(symbol_data)[time_input,3]
    High <- coredata(symbol_data)[time_input,2]
    Volume <- coredata(symbol_data)[time_input,5]/1000000
    Time <- time(symbol_data)[time_input]
    f <- list(family = "Courier New, monospace", size = 18, color = "#7f7f7f")
    xaxislabel <- list(title = "Date")
    y_range <- round_any(max(High)-min(Low),1)
    ##Y-Axis Range
    if(round_any(max(High)-min(Low),1)<10){
      tick_levels = round_any(round_any(min(Low),1)+seq(0,10,1),1,floor)
    } else if(round_any(max(High)-min(Low),1)<50){
      tick_levels = round_any(round_any(min(Low),1)+seq(0,50,5),5,floor)
    } else if(round_any(max(High)-min(Low),1)<100){
      tick_levels = round_any(round_any(min(Low),1)+seq(0,100,10),5,floor)
    } else{  y_range <- round_any(max(High)-min(Low),1)
    tick_levels = round_any(round_any(min(Low),1)+seq(0,y_range,y_range/10),5,floor)}
    yaxislabel <- list(title = "Price",range=c(min(tick_levels)-y_range/3,max(High)),tickmode="array",tickvals= tick_levels)
    ######CANDLES##############
    if(input$chart_type=="candlesticks"){
      pos <- vector(length=length(time_input))
      pos[which(Close>Open)] <- "Positive"
      pos[which(Open>Close)] <- "Negative"
      pos[which(Open==Close)] <- "No Change"
      #Candle Bars - Positive
      
      Thick <- 560/length(Time)
      p <- plot_ly(x=Time[which(pos=="Positive")],
                   y = pmin(Open[which(pos=="Positive")],Close[which(pos=="Positive")]),
                   error_y = list(type="data",symmetric=FALSE, array=c(abs(Open[which(pos=="Positive")]-Close[which(pos=="Positive")])),
                                  arrayminus=c(Close[which(pos=="Positive")]-Close[which(pos=="Positive")]),width=0,thickness=Thick,color = "Green"),
                   mode = "markers",opacity=0,showlegend=F,
                   text = paste(Time,paste("Open: ",Open[which(pos=="Positive")],sep=""),
                                paste("High: ",High[which(pos=="Positive")],sep=""),
                                paste("Low: ",Low[which(pos=="Positive")],sep=""),paste("Close: ",
                                                                                        Close[which(pos=="Positive")],sep=""),paste("Volume:",Volume[which(pos=="Positive")],sep=""),sep="<br>"),marker = list(color = "Green"),hoverinfo="text")
      #Candle Bars - Negative
      p2 <- add_trace(p,x=Time[which(pos=="Negative")],
                      y = pmin(Open[which(pos=="Negative")],Close[which(pos=="Negative")]),
                      error_y = list(type="data",symmetric=FALSE, array=c(abs(Open[which(pos=="Negative")]-Close[which(pos=="Negative")])),
                                     arrayminus=c(Close[which(pos=="Negative")]-Close[which(pos=="Negative")]),width=0,thickness=Thick,color = "Red"),
                      mode = "markers",opacity=0,text =
                        paste(Time,paste("Open: ",Open[which(pos=="Negative")],sep=""),
                              paste("High: ",High[which(pos=="Negative")],sep=""),
                              paste("Low: ",Low[which(pos=="Negative")],sep=""),paste("Close: ",
                                                                                      Close[which(pos=="Negative")],sep=""),paste("Volume: ",Volume[which(pos=="Negative")],sep=""),sep="<br>"), marker = list(color="Red"),hoverinfo="text")
      # #Wicks - Positive
      p3 <-add_trace(p2,x=Time[which(pos=="Positive")],
                     y = pmax(Open[which(pos=="Positive")],Close[which(pos=="Positive")]),
                     error_y = list(type="data",symmetric=FALSE, array=c(High[which(pos=="Positive")]-pmax(Open[which(pos=="Positive")],Close[which(pos=="Positive")])),
                                    arrayminus=c(pmax(Open[which(pos=="Positive")]-Low[which(pos=="Positive")],Close[which(pos=="Positive")]-Low[which(pos=="Positive")])),
                                    width=0,thickness=Thick/3.5,color = "Green"),hoverinfo="none")
      #Wicks - Negative
      p4 <-add_trace(p3,x=Time[which(pos=="Negative")],
                     y = pmax(Open[which(pos=="Negative")],Close[which(pos=="Negative")]),
                     error_y = list(type="data",symmetric=FALSE, array=c(High[which(pos=="Negative")]-pmax(Open[which(pos=="Negative")],Close[which(pos=="Negative")])),
                                    arrayminus=c(pmax(Open[which(pos=="Negative")]-Low[which(pos=="Negative")],Close[which(pos=="Negative")]-Low[which(pos=="Negative")])),
                                    width=0,thickness=Thick/3.5,color = "Red"),hoverinfo="none")%>% 
        ####Layout x,y,y2
        # vol_range <- max(Volume)-min(Volume)
        layout(xaxis=xaxislabel,yaxis=yaxislabel,autosize=F,width=1250,height=550,
               yaxis2 = list(overlaying = "y", side = "right",
                             title = "Volume",titlefont = list(color = "Green"),range=c(min(Volume),max(Volume)*4),tickmode="array",tickvals=(max(Volume)*seq(0,1,.2)-((max(Volume)*seq(0,1,.2))%%100)))
               ,hovermode="closest",range=c(min(Time),max(Time)))
      ####Volume
      p5<-add_trace(p4,x=Time,y=Volume,yaxis="y2",fill="tonexty",mode="markers",marker=list(opacity=0), error_y=list(opacity=0),
                    text=paste(Time,paste("Open: ",Open,sep=""),paste("High: ",High,sep=""),paste("Low: ",Low,sep=""),paste("Close: ",Close,sep=""),paste("Volume: ",Volume,sep=""),sep="<br>"),hoverinfo="text",fillcolor="rgba(43, 46, 47, 0.45)")
      p6 <- add_trace(p5,x=Time,y=rep(max(Volume),length(Volume)),yaxis="y2",fill="tozerox",mode="lines",error_y=list(opacity=0),fillcolor="Black",hoverinfo="none",line=list(color="Black"))
      # vol <-plot_ly(x = Time,y = Volume/1000000)%>%
      #   layout(xaxis = list(domain = c(0, 1)),
      #          yaxis = list(domain = c(0, .45)),width=1300,height=650)
      # subplot(p4,vol,nrows = 2)
      #######END CANDLES###############
    } else if(input$chart_type=="line"){ 
      Holt <- fitted.values(holt(Close, initial="optimal"))
      l1 <- plot_ly(x=Time,y=Close) %>% layout(xaxis=xaxislabel)
      l2 <- add_trace(x=Time,y=Holt)
    }
  }
  # output$structure<-renderText({str(input$Ticker)})
  
  output$Ticker_Plot<-renderPlot({make_chart(input$Ticker)})
  
  output$PLOTLY <- renderPlotly({ make_plotly(input$Ticker)})
  
  #EOS
})