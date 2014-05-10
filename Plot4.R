Plot4 <- function()
{
  consumption <- read.table("household_power_consumption.txt",sep=";",header=TRUE,na.strings=c('?'))
  consumption[,1] <- sapply(consumption[,1],as.character)
  
  req_dates <- consumption[grep("^[1-2]/2/2007$",consumption$Date),]
  req_dates[,3:9] <- sapply(req_dates[,3:9],as.numeric)
  
  library(lubridate)
  req_dates$datetime <- paste(req_dates$Date," ",req_dates$Time)
  req_dates$datetime <- dmy_hms(req_dates$datetime)
  req_dates$day <- weekdays(req_dates$datetime)
  
  par(mfrow=c(2,2))
  with(req_dates,
{
  plot(datetime,Global_active_power,type="l",ylab="Global Active Power",xlab="")
  plot(datetime,Voltage,type="l",ylab="Voltage",xlab="datetime")
  plot(datetime,Sub_metering_1,type="l",ylab="Energy sub metering",xlab="")
  lines(req_dates$datetime,(req_dates$Sub_metering_2),lwd=1,col="red")
  lines(req_dates$datetime,(req_dates$Sub_metering_3),lwd=1,col="blue")
  legend("topright",lty=1,cex=0.5,bty ="n",col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_3","Sub_metering_3"))
  plot(datetime,Global_reactive_power,type="l",ylab="Global_reactive_power",xlab="datetime")
})
  dev.copy(png,filename="plot4.png",height=480, width=480)
  dev.off()
}
