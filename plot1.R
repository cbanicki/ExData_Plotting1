plot1 <- function() {
  #Plot the frequency (y-axis) of Global Active Power
  #in a red bar chart
  #with Global Active Power on the x-axis
  #displayed in increments of 2, starting at 0
  
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
   mutate(GW = floor(Global_active_power/.50)*.50) 

#GAP

  # Bar Plot 
    counts <- table(GAP$GW)
    
    png("plot1.png")
    
    barplot(counts, main="Global Active Power", 
        xlab="Global Active Power (kilowatts)", ylab="Frequency", beside=TRUE, col="red")
    
    # dev.copy(png,"plot1.png",width=480,height=480,units="in",res=100)
    dev.off()

  
  }

  



  
 

 