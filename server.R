source('functions.R')

Quandl.api_key("jU9SSmUJFjivxgnYcFLp")

QC <- read.csv('CodeList.csv',stringsAsFactors = F)

QC <- data.frame(Code = sapply(QC[,1],attach), Name = QC$Name, stringsAsFactors = F)
QC <- setNames(split(QC, seq(nrow(QC))), QC$Name)

# Get EurekaHedge data from Quandl and transform to % term
returns <- returns.data.frame(lapply(QC, read.quandl))
returns[sapply(returns, is.numeric)] <- returns[sapply(returns, is.numeric)] / 100

# Get SP500 index returns
SPX <- Quandl("CHRIS/CME_SP1", transform="rdiff", collapse="monthly", start_date="2000-01-01")
SPX <- data.frame(SPX$Date, SPX$Settle)
colnames(SPX) <- c('Date', 'SPX')

returns <- left_join(SPX, returns, by = 'Date')

# arrange the returns by Date, format to decimal, add 0s at first row
returns <- na.omit(arrange(returns, Date))

shinyServer(function(input, output, session){

    output$index <- renderPlot({
      
    # Select starting time for performance 
    monthBack <- input$TimeSelector
      
    # Select dates of interest
    returns <- filter(returns, Date >= Sys.Date() %m-% months(monthBack))
    
    # Add zeros to make index start from 1
    returns <- add_zeros(returns)
    
    # Create the value index
    perf <- data.frame(Date = returns[,1], cumprod(1+returns[,-1]))
    
    # Chart performance curves
    melt.perf <- melt(perf, id = 'Date')
    
    ggplot(data = melt.perf, aes(x = Date, y = value,  group=variable)) +
      geom_line(aes(colour=variable)) +
      geom_text(data = filter(melt.perf, Date == max(melt.perf$Date)), aes(label=variable)) + 
      labs(y = "Performance Index", title = paste0(monthBack, "-month Hedge Fund Performance Curve")) + 
      theme(legend.position="none")
    })
  
  # output$table <- renderTable({
  #   dt <- arrange(melt(snapshot, id = 'Date')[2:3], -value)
  #   colnames(dt) <- c('Strategy', 'Return')
  #   dt
  # })
  
  
  output$sorted <- renderPlot({
    
    monthBack <-  input$MonthSelector
    
    # most recent returns
    # snapshot <- tail(returns, 1)
    snapshot <- returns[nrow(returns) - monthBack, ]
    lastDate <- returns[nrow(returns) - monthBack, 1]
    
    
    # In order to display on ggplot we need to melt the returns data frame
    melt.snapshot <- melt(snapshot, id='Date')
    
    ggplot(data = melt.snapshot, aes(x = reorder(variable, value), y = value,  group=value)) +
      geom_bar(stat = "identity", position="dodge") +
      geom_text(aes(label=percent(value))) + 
      labs(y = "Return (%)", title = paste0("Performance Ranking as of ", lastDate), x = "Strategy") + 
      coord_flip()
  })
  
  output$correlation <- renderPlot({
    
    corr <- round(cor(returns[,-1]),2)
    corr <- get.upper.tri(corr)
    corr <- melt(corr, na.rm = TRUE)
    
    # Create a ggheatmap
    ggheatmap <- ggplot(corr, aes(Var2, Var1, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal() + # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
      coord_fixed()
    
    ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal") +
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))
  })
})