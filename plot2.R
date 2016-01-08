plot2 <- function() {
  #Plot Global Active Power (Kilowatts) in a line graph
  #with Weekday Names on the x-axis
  #and the y-axis in increments of 2, starting from 0
  
  
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
   #mutate(Day = weekdays(Pwr$Date)) %>%
   mutate(DateTime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")) %>%
   #mutate(Day = format(seq.Date(as.Date(Date), by = 'day', len = 1), "%a")) %>%
   select(Date, DateTime, Global_active_power) %>%
   group_by(Date)
    
 
    counts <- c(GAP$Global_active_power)
    
    day <- c(GAP$DateTime)
    
    dateShort <- c(GAP$Date)
    
    MaxPower <- max(counts)
 
    
    # Get all the weekdays for the datetimes in the data
#     weekday <- c(1:length(day))
#     for (i in 1:length(day) ) {
#       weekday[i] <- format(seq.Date(as.Date(day[i]), by = 'day', len = 1), "%a")
#     }
#     
#     weekday <- unique(weekday)
            
          
         
         plot(day,counts,ylim=c(0,MaxPower),axes=FALSE,ylab= "Global Active Power (kilowatts)", type='l', col='black',xaxt = "n") 
         #
         points(x=GAP$Date, y=counts, col='black', type='l', lwd=2)
         
         axis.POSIXct(1, day, format="%a") #%m/%d") 
        
         #y-axis labels
         axis(2, las=1, at=2*0:MaxPower)
         
    
    # Create box for graph border
    box()

    
#     dev.copy(png,"plot2.png",width=480,height=480,units="in",res=100)
#     dev.off()
# 
#   
  }

  



  
 

 