plot4 <- function() {
  #Plot 4 graphs
  
  
  library(dplyr)  
  
  require(data.table)
  
  library(datasets)
  
  # library(lubridate)
  
  
  #setwd("C://R//ExploratoryAnalytics//Proj1")
  
  
  if (!file.exists("exdata%2Fdata%2Fhousehold_power_consumption.zip")) {
    
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    
    fileName <- "exdata%2Fdata%2Fhousehold_power_consumption.zip"
    
    download.file(fileURL, fileName, mode = "wb")
    
    dateDownloaded <- date()
    
    unzip(fileName, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = ".", unzip = "internal",
          setTimes = FALSE)
  }
  
  # Import text file into data frame
  hhPwr <- tbl_df(fread("household_power_consumption.txt",header = TRUE,na.strings = c("?")))
  
  # head(hhPwr)
  
  # Take only data from 2/1 and 2/2 in 2007
  Pwr <- subset(hhPwr, hhPwr$Date %in% c('1/2/2007','2/2/2007')) 
  
  # Remove original data set with extra dates
  rm("hhPwr")
  
  # Change Date field format from String to Date
  Pwr$Date <- as.Date(Pwr$Date, format="%d/%m/%Y")
  

  d0 <- matrix(rnorm(45), ncol=3)
  d1 <- matrix(rnorm(45), ncol=3)
  d2 <- matrix(rnorm(45), ncol=3)
  d3 <- matrix(rnorm(45), ncol=3)
  
  
  # Create a 2 by 2 vector to be used for the 4 graphs
  par(mfrow = c(2,2))
  
  # Using base R graphics
  limits <- range(d0,d1)
  
  # Get processed subset of data for graphs
  myData <- 
    Pwr %>%
    mutate(DateTime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")) %>%
    #Rounding the results up to the nearest 0.25
    mutate(GW = floor(Global_active_power/.50)*.50) %>%
    select(Date,DateTime,Global_active_power,Voltage,Sub_metering_1,Sub_metering_2,Sub_metering_3,Global_reactive_power)
  
  # **************************************************************************************************************************************
  #                                                Global Active Power Graph
  #***************************************************************************************************************************************
  
  GAP <- c(myData$Global_active_power)
  
  day <- c(myData$DateTime)
  
  dateShort <- c(myData$Date)
  
  MaxPower <- max(GAP)

  png("plot4.png", width=480,height=480)   
  
  
  par(mfrow= c(2, 2))
  

  
  # Global Active Power Graph
  plot(day,GAP,ylim=c(0,MaxPower),axes=FALSE,ylab= "Global Active Power (kilowatts)", type='l', col='black',xaxt = "n") 
  #
  points(x=dateShort, y=myData$GW, col='black', type='l', lwd=2)
  
  axis.POSIXct(1, day, format="%a") #%m/%d") 
  
  #y-axis labels
  axis(2, las=1, at=2*0:MaxPower)
  
  
  # Create box for graph border
  box()
  
  # **************************************************************************************************************************************
  #                                                 Voltage Graph
  #***************************************************************************************************************************************
  
  MaxVoltage <- max(myData$Voltage)
  
  
  MinVoltage <- min(myData$Voltage)
  
  plot(day,myData$Voltage,ylim=c(MinVoltage,MaxVoltage),axes=FALSE,ylab= "Voltage", type='l', col='black',xaxt = "n") 
  #
  points(x=dateShort, y=myData$Voltage, col='black', type='l', lwd=2)
  
  axis.POSIXct(1, day, format="%a") #%m/%d") 
  
  #y-axis labels
  axis(2, las=1, at=seq(ceiling(MinVoltage),ceiling(MaxVoltage), by = 4))
  
  
  
  # Create box for graph border
  box()
  
  
  # **************************************************************************************************************************************
  #                                                 Sub Metering Graph
  #***************************************************************************************************************************************
  
  
  # max(GAP$Sub_metering_1,GAP$Sub_metering_2,GAP$Sub_metering_3)
  counts <- c(max(myData$Sub_metering_1,myData$Sub_metering_2,myData$Sub_metering_3))
  
  Pcolors <- c(rgb(r=0.0,g=0.0,b=0.9), "blue", "red", "gray")
  
  day <- c(myData$DateTime)
  
  
  gapRange <- range(0,counts)
  
  #png("plot3.png", width=480,height=480)     
  
  plot(day, Pwr$Sub_metering_1, type="l", ylab= "Energy sub metering", col="gray", ylim=gapRange,
       axes=FALSE, ann=TRUE)
  
  #y-axis labels
  axis(2, las=1, at=10*0:(counts - counts %% 10))
  
  lines(day,Pwr$Sub_metering_2, type="l", col="red")
  
  lines(day, Pwr$Sub_metering_3, type="l", col="blue")
  
  axis.POSIXct(1, day, format="%a") #%m/%d") 
  
  
  legend("topright", 
         inset=.05, 
         cex = 1, 
         c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
         horiz=FALSE, 
         lty=c(1,1), 
         lwd=c(2,2), 
         col=c("gray","red","blue"), 
         bg="white",
         text.font=3)
  
  
  
  # Create box for graph border
  box()
  
  
  # **************************************************************************************************************************************
  #                                               Global Reactive Power Graph
  #***************************************************************************************************************************************
  
  MaxGRP <- max(myData$Global_reactive_power)
  
  
  MinGRP <- min(myData$Global_reactive_power)
  
  plot(day,myData$Global_reactive_power,ylim=c(MinGRP,MaxGRP),axes=FALSE,ylab= "Global_reactive_power", type='l', col='black',xaxt = "n") 
  #
  points(x=dateShort, y=myData$Global_reactive_power, col='black', type='l', lwd=2)
  
  axis.POSIXct(1, day, format="%a") #%m/%d") 
  
  #y-axis labels
  axis(2, las=1, at=seq(ceiling(MinGRP),ceiling(MaxGRP), by = .1))
  
  #floor(Global_active_power/.50)*.50)
  
  # Create box for graph border
  box()
  
  #     dev.copy(png,"plot4.png",width=480,height=480,units="in",res=100)
   dev.off()
  # 
  #   
}








