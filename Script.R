library(Quandl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)

read.quandl <- function(qcode){
  data <- Quandl(qcode$Code)
  colnames(data) <- c('Date', qcode$Name)
  data
}

returns.data.frame <- function(x){
  df <- x[[1]]
  for (i in seq(x)[-1]){
    df <- full_join(df, x[[i]], by = 'Date')
  }
  return(df)
}

to.xts <- function(x){
  x1 <- as.xts(x[,-1],order.by = x[,1])
}
attach <- function(x) paste(BASE, x, sep='/')

add_zeros <- function(x){
  zeros <- x[1,1:ncol(x)]
  zeros$Date <- zeros$Date %m-% months(1)
  zeros[,2:ncol(zeros)] <- 0
  return(rbind(zeros, x))
}


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

ret <- add_zeros(returns)

# Select returns of interest
# ret <- filter()

# Create the value index
perf <- data.frame(Date = ret[,1], cumprod(1+ret[,-1]))

snapshot <- filter(returns, Date == '2016-12-31')
recent <- filter(returns, Date >= Sys.Date() %m-% months(6))

# In order to display on ggplot we need to melt the returns data frame
melt.snapshot <- melt(snapshot, id='Date')

ggplot(data = melt.snapshot, aes(x = reorder(variable, -value), y = value,  group=value)) +
  geom_bar(stat = "identity", position="dodge") +
  labs(y = "Return (%)", title = "Hedge Fund Performance in " , x = "Strategy") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Correlation Heatmap
melt.corr <- melt(cor(na.omit(returns[,-1])))
ggplot(melt.corr, aes(Var1, Var2)) +   
  geom_tile(aes(fill = value)) + 
  scale_fill_gradient(low="green", high="red") +
  labs(title = "Correlation Heatmap") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title = element_blank(), legend.position="none")

# Chart performance curves
melt.perf <- melt(perf, id = 'Date')

ggplot(data = melt.perf, aes(x = Date, y = value,  group=variable)) +
  geom_line(aes(colour=variable)) +
  labs(y = "Performance Index", title = "Hedge Fund Performance Curve ")

# Return Distribution
melt.returns <- melt(returns, id = 'Date')

ggplot(data = melt.returns[,-1], aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill = variable)) +
  labs(title = "Return dispersion", y = "Return") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), axis.title.x = element_blank(), legend.position="none")

ggplot(data = melt.returns[,-1], aes(value)) + 
  geom_histogram(aes(fill = variable)) +
  facet_wrap(~variable) +
  labs(title = "Return distribution") +
  theme(axis.title.x = element_blank(), legend.position="none")

