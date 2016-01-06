plot1 <- function() {
  
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
   #Rounding the results up to the nearest 0.25
   #mutate(GW = floor(Global_active_power/.50)*.50) %>%
   mutate(Day = weekdays(Pwr$Date)) 
   
 
  WeekDays <- unique(GAP$Day)
  

#GAP

  # Bar Plot 
    counts <- c(Pwr$Global_active_power)
    
    gapRange <- range(0,counts)
    
    plot(counts, type="l", col="black", ylim=gapRange,
         axes=FALSE, ann=TRUE)
    
    axis(1, lab="Global Active Power (kilowatts)")
    
    
    text(axTicks(1), 
         labels=c(WeekDays),
         xpd=T, cex=0.8)
    
    # Create box for graph border
    box()
    
    
#     dev.copy(png,"plot2.png",width=8,height=6,units="in",res=100)
#     dev.off()
# 
#   
  }

  



  
 

 