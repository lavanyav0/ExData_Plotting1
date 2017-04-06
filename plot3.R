library(dplyr)

read_data <- function() {
  dataset <- read.csv("household_power_consumption.txt",sep=";")
  dataset <- filter(dataset,Date=="1/2/2007"|Date=="2/2/2007")
  dataset$Time <- strptime(paste(dataset$Date,dataset$Time,sep=" "),"%d/%m/%Y %H:%M:%S")
  dataset$Date <- as.Date(dataset$Date,"%d/%m/%Y")
  dataset$Global_active_power <- as.numeric(as.vector(dataset$Global_active_power))
  dataset$Sub_metering_1 <- as.numeric(as.vector(dataset$Sub_metering_1))
  dataset$Sub_metering_2 <- as.numeric(as.vector(dataset$Sub_metering_2))
  dataset$Sub_metering_3 <- as.numeric(as.vector(dataset$Sub_metering_3))
  dataset
}

plot3 <- function() {
  dataset <- read_data()
  with(dataset,plot(Time,Sub_metering_1,type="l",xlab="",ylab="Energy sub metering"))
  with(dataset,points(Time,Sub_metering_2,col="red",type="l"))
  with(dataset,points(Time,Sub_metering_3,col="blue",type="l"))
  legend("topright",lty=c(1,1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  dev.copy(png,file="plot3.png",width=480,height=480)
  dev.off()
}