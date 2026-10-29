library(Quandl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(scales)
library(ggthemes)

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

# Get upper triangle of the correlation matrix
get.upper.tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}