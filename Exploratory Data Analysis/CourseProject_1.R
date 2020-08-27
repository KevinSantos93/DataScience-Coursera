filename <- "Electric Power Consumption.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'
  download.file(fileURL, filename, method="curl")
}  

  # Checking if folder exists
if (!file.exists("Electric Power Consumption")) { 
  unzip(filename) 
}

library(data.table)
library(dplyr)

#Read the datset
data <- read.table("household_power_consumption.txt", sep = ";", header = TRUE, col.names = c('Date', 'Time', 'GlobalActivePower', 'GlobalReactivePower', 'Voltage', 'GlobalIntensity', 'SubMetering_1', 'SubMetering_2', 'SubMetering_3'))
str(data)

#Subset data from 1/2/2007 to 2/2/2007
subData <- subset(data, data$Date == '1/2/2007' | data$Date == '2/2/2007')
str(subData)

#Plot 1
hist(as.numeric(subData$GlobalActivePower), col = 'red', main = 'Global Activity Power',
     xlab = 'Global Active Power (kilowatts)')
dev.copy(png, file = 'plot1.png') #Copy my plot to a png file
dev.off() #Close Graphic device

#Set Date variable as datetime
subData$Date <- as.Date(subData$Date, format = '%d/%m/%Y')
subData$Time <- strptime(subData$Time, format = '%H:%M:%S')
subData[1:1440,"Time"] <- format(subData[1:1440,"Time"],"2007-02-01 %H:%M:%S")
subData[1441:2880,"Time"] <- format(subData[1441:2880,"Time"],"2007-02-02 %H:%M:%S")

#Plot 2
plot(subData$Time, as.character(subData$GlobalActivePower), type = 'l',
     xlab = '', ylab = 'Global Active Power (kilowatts)')
dev.copy(png, file = 'plot2.png') #Copy my plot to a png file
dev.off() #Close Graphic device

#Plot 3
plot(subData$Time, as.character(subData$SubMetering_1), type = 'l',
     xlab = '', ylab = 'Energy sub metering')
points(subData$Time, as.character(subData$SubMetering_2), type = 'l',
     xlab = '', ylab = 'Energy sub metering', col = 'red')
points(subData$Time, as.character(subData$SubMetering_3), type = 'l',
       xlab = '', ylab = 'Energy sub metering', col = 'blue')
legend('topright', col = c('black', 'red', 'blue'),
       legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
       lty = 1, cex = 0.6)
dev.copy(png, file = 'plot3.png') #Copy my plot to a png file
dev.off() #Close Graphic device

#Plot 4

#Global Active Power plot
par(mfcol = c(2,2), mar = c(2.5,4,1,1))
plot(subData$Time, as.character(subData$GlobalActivePower), type = 'l',
     xlab = '', ylab = 'Global Active Power')

#Submetering plot
plot(subData$Time, as.character(subData$SubMetering_1), type = 'l',
     xlab = '', ylab = 'Energy sub metering')
points(subData$Time, as.character(subData$SubMetering_2), type = 'l',
       xlab = '', ylab = 'Energy sub metering', col = 'red')
points(subData$Time, as.character(subData$SubMetering_3), type = 'l',
       xlab = '', ylab = 'Energy sub metering', col = 'blue')
legend('topright', col = c('black', 'red', 'blue'),
       legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
       lty = 1, cex = 0.6)

#Voltage plot
plot(subData$Time, as.character(subData$Voltage), type = 'l',
     xlab = 'datetime', ylab = 'Voltage')

#Global Reactive Power plot
plot(subData$Time, as.character(subData$GlobalReactivePower), type = 'l',
     xlab = 'datetime', ylab = 'Global Reactivity Power')
dev.copy(png, file = 'plot4.png') #Copy my plot to a png file
dev.off() #Close Graphic device

