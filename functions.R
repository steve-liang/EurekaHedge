# list.of.packages <- c("ggplot2", "Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(Quandl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(scales)
library(ggthemes)
library(PerformanceAnalytics)
library(quantmod)

BASE <- 'EUREKA'

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

rank.strategies <- function(x){
  data.frame("Date"=x[,1], t(apply(-x[,-1], 1, rank, ties.method = 'min')))
}

