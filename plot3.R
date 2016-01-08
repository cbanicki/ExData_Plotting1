plot3 <- function() {
  #Plot Energy sub metering (1,2, and 3) in a line graph
  #with values on the y-axis (in increments of 10 starting at zero)
  #and the weekday names on the x-axis
  #Include a legend for each sub metering variable
  
  library(dplyr)  
  
  require(data.table)
  
  library(datasets)
  
 # library(lubridate)
  
  
  setwd("C://R//ExploratoryAnalytics//Proj1")
  

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
  
  GAP <- 
    Pwr %>%
    mutate(DateTime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")) %>%
    select(Date, DateTime, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
    group_by(Date)
  
  # max(GAP$Sub_metering_1,GAP$Sub_metering_2,GAP$Sub_metering_3)
  counts <- c(max(GAP$Sub_metering_1,GAP$Sub_metering_2,GAP$Sub_metering_3))
  
  Pcolors <- c(rgb(r=0.0,g=0.0,b=0.9), "blue", "red", "gray")
     
     day <- c(GAP$DateTime)
     
    
    gapRange <- range(0,counts)
    
    png("plot3.png", width=480,height=480)     
    
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
    
    
    dev.off()
    
    
  }

  



  
 

 