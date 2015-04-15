
main <- function(){
  ## Read in data
  mydata <- read.csv("C:/Users/abaum/Documents/Analytics/Day3/stock.csv")
     
         
  ## Change time to correct format
  mydata$time <- as.POSIXct(mydata$time, format="%H:%M:%S")
  
  head(mydata)
  
  
  ## Only need data between 9:30AM and 9:45
  startTime <- as.POSIXct("9:30:00", format="%H:%M:%S")
  endTime <- as.POSIXct("9:45:00", format="%H:%M:%S")
  
  head(startTime)
  
  head(endTime)
  
  mydata <- mydata[(mydata$time > startTime),]
  mydata <- mydata[(mydata$time < endTime),]
  mydata <- mydata[(mydata$date == "3/12/2015"),]
  

  ## Select subset of data: time and volatility, time and delta
  
  vola <- mydata[c(2,7)]  
  
  head(vola)
  delta <- mydata[c(2,8)]
    
  library(ggplot2)
  ## Plot volatility time series chart using ggplot  
  ggplot(data=vola, aes(x=time, y=volatility)) + geom_line()
  
}


head(vola)
class(vola)
new <- aggregate(vola, force, mean)
new <- vola[,data_table(avg=mean(volatility)),by=hour]


library(dplyr)

viola1 <- group_by(DeviceTime = cut(DeviceTime, breaks="5 min")) %.%
  dplyr::summarize(Concentration = mean(Concentration))

library(TTR)
class(vola)
volaTS <- ts(vola)
volaTS

class(volaTS)

moving <- SMA(volaTS, n=10)

moving
ggplot(moving)


