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

plot2 <- function() {
  dataset <- read_data()
  with(dataset,plot(Time,Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)"))
  dev.copy(png,file="plot2.png",width=480,height=480)
  dev.off()
}