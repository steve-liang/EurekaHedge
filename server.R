source('functions.R')

Quandl.api_key("jU9SSmUJFjivxgnYcFLp")

BASE <- 'EUREKA'
QC <- read.csv('CodeList.csv',stringsAsFactors = F)

QC <- data.frame(Code = sapply(QC[,1],attach), Name = QC$Name, stringsAsFactors = F)
QC <- setNames(split(QC, seq(nrow(QC))), QC$Name)

# Get raw data from Quandl
returns <- returns.data.frame(lapply(QC, read.quandl))

# arrange the returns by Date, format to decimal, add 0s at first row
returns <- arrange(returns, Date)
returns[sapply(returns, is.numeric)] <- returns[sapply(returns, is.numeric)] / 100

shinyServer(function(input, output, session){

  output$index <- renderPlot({
    # Select dates of interest
    returns <- filter(returns, Date >= Sys.Date() %m-% months(24))
    
    # Add zeros to make index start from 1
    returns <- add_zeros(returns)
    
    # Create the value index
    perf <- data.frame(Date = returns[,1], cumprod(1+returns[,-1]))
    
    # Chart performance curves
    melt.perf <- melt(perf, id = 'Date')
    
    ggplot(data = melt.perf, aes(x = Date, y = value,  group=variable)) +
      geom_line(aes(colour=variable)) +
      geom_text(data = filter(melt.perf, Date == max(melt.perf$Date)), aes(label=variable)) + 
      labs(y = "Performance Index", title = "24-month Hedge Fund Performance Curve") + 
      theme(legend.position="none")
  })
  
  # output$table <- renderTable({
  #   dt <- arrange(melt(snapshot, id = 'Date')[2:3], -value)
  #   colnames(dt) <- c('Strategy', 'Return')
  #   dt
  # })
  
  
  output$sorted <- renderPlot({
    
    # most recent returns
    snapshot <- tail(returns, 1)
    
    # In order to display on ggplot we need to melt the returns data frame
    melt.snapshot <- melt(snapshot, id='Date')
    
    ggplot(data = melt.snapshot, aes(x = reorder(variable, value), y = value,  group=value)) +
      geom_bar(stat = "identity", position="dodge") +
      geom_text(aes(label=percent(value))) + 
      labs(y = "Return (%)", title = "Performance Ranking", x = "Strategy") + 
      coord_flip()
  })
  
  
}
)