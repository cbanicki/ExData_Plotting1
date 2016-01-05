Proj1 <- function() {
  
  library(dplyr)  
  
  require(data.table)
  
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
  
  hhPwr <- fread("household_power_consumption.txt",header = TRUE,na.strings = c("?"))
  
  #head(hhPwr)
  
  Pwr <- tbl_df(hhPwr)
  
  rm("hhPwr")
  
  
  Pwr$Date <- as.Date(Pwr$Date, format="%d/%m/%Y")
  
 GAP <- 
   Pwr %>%
   summarize(n=n()) %>%
   select(Date, Global_active_power) %>%
   #subset(Pwr$Date %in% c('1/2/2007','2/2/2007')) 
   filter(Date == '1/2/2007' | Date == '2/2/2007')

GAP
# 
#   # Bar Plot 
#     frequency <- table(mtcars$gear)
#     barplot(counts, main="Car Distribution", 
#         xlab="Number of Gears")
  
  
  }

  



  
 

 