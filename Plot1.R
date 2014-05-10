Plot1 <- function()
{
  consumption <- read.table("household_power_consumption.txt",sep=";",header=TRUE,na.strings=c('?'))
  consumption[,1] <- sapply(consumption[,1],as.character)
  
  req_dates <- consumption[grep("^[1-2]/2/2007$",consumption$Date),]
  req_dates[,3:9] <- sapply(req_dates[,3:9],as.numeric)
  
  library(lubridate)
  req_dates$datetime <- paste(req_dates$Date," ",req_dates$Time)
  req_dates$datetime <- dmy_hms(req_dates$datetime)
  req_dates$day <- weekdays(req_dates$datetime)
  
  with(req_dates,hist(Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)"))
  dev.copy(png,filename="plot1.png",height=480, width=480)
  dev.off()
}
