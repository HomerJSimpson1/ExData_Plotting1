plot2 <- function(strFileName="household_power_consumption.txt") {
  ## Exploratory Data Analysis Coursera Class - July 2014 Session
  ## Project 1 - Due Date July 13, 2014
  ## File: plot2.R
  
  ## Get the data.  Uses a "helper" function (see below).
  df <- readdata(strFileName)

  ## Plot Global Active Power over time.
  plot(df$DateTime, df$Global_active_power, ylab="Global Active Power (kilowatts)", xlab="", type="l")

  ## Save the file as a PNG file with a width of 480 pixels and a height of 480 pixels.
  dev.print(device=png, file='plot2.png', width=480, height=480, units='px')
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
