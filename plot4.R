plot4 <- function(strFileName="household_power_consumption.txt") {
  ## Exploratory Data Analysis Coursera Class - July 2014 Session
  ## Project 1 - Due Date July 13, 2014
  ## File: plot4.R
  
  ## Get the data.  Uses a "helper" function (see below).
  df <- readdata(strFileName)

  par(mfrow = c(2,2))

  ## Upper left plot. Plot Global Active Power over time.
  plot(df$DateTime, df$Global_active_power, ylab="Global Active Power", xlab="", type="l")

  ## Upper right plot.  Plot of Voltage over time.
  plot(df$DateTime, df$Voltage, ylab="Voltage", xlab="datetime", type="l")  
  
  ## Lower left plot. Plot Energy sub metering over time.  
  plot(df$DateTime, df$Sub_metering_1, ylab="Energy sub metering", xlab="", type="l")
  lines(df$DateTime, df$Sub_metering_2, ylab="Energy sub metering", xlab="", type="l", col="red")
  lines(df$DateTime, df$Sub_metering_3, ylab="Energy sub metering", xlab="", type="l", col="blue")
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),lty=c(1,1), col=c("black", "red", "blue"),lwd=c(2.5, 2.5, 2.5), cex=0.92, inset = 0.01, bty="n")

  ## Lower right plot.  Plot of Global Reactive Power over time.
  plot(df$DateTime, df$Global_reactive_power, ylab="Global_reactive_power", xlab="datetime", type="l")

  ## Save the file as a PNG file with a width of 480 pixels and a height of 480 pixels.
  dev.print(device=png, file='plot4.png', width=480, height=480, units='px')
  dev.off()
}




readdata <- function(strFileName="household_power_consumption.txt") {
  ## Exploratory Data Analysis Coursera Class
  ## Project 1

  ## Read the data from a text file into an R data frame.
  hpcdat <- read.table(strFileName,sep=";",header=TRUE, stringsAsFactors=FALSE, na.strings="?", comment.char="",
                       colClasses = c("character", "character", "numeric", "numeric","numeric", "numeric","numeric", "numeric", "numeric"))

  ## Select only the dates 2/1/2007 and 2/2/2007, as per the instructions for the assignment.
  hpcdat <- hpcdat[(hpcdat$Date == "1/2/2007" | hpcdat$Date=="2/2/2007"),]
  
  ## Combine the date and time string (character) fields into one field (type string also)
  hpcdatetime <- paste(hpcdat$Date, hpcdat$Time)

  ## Convert the string vector into a POSIXlt class object
  hpcdatetime <- strptime(hpcdatetime, format="%d/%m/%Y %H:%M:%S")

  ## Add the POSIXlt vector to the data frame (as the new first column in the enhanced data frame)
  hpcdat <- cbind(hpcdatetime, hpcdat)

  ## Rename the new column
  names(hpcdat)[1] <- "DateTime"
  
  ## For some reason, this converts the POSIXlt vector to a POSIXct column.  Not sure why?
  ## Use the as.POSIXlt() function to ensure this doesn't happen.
  hpcdat$DateTime <- as.POSIXlt(hpcdat$DateTime)  

  ## Select only cases without NA values
  hpcdat <- na.omit(hpcdat)
  
  return(hpcdat)
}
