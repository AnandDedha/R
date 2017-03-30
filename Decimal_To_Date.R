library(DAAG) #Load DAAG packages
library(lubridate)    #Load lubridate packages. It is used work the dates and times
data(jobs)            #Load the data
print(jobs)
Alberta <- c(1:24)
BC <- c(1:24)
df<- data.frame(Alberta, BC) # A data frame is created with the name df using  two vectors Alberta & BC
combined_jobs <- cbind(jobs,df) # Cbind to combine the cloumns of two data frames
print(combined_jobs)
jobs$Date <- format(date_decimal(jobs$Date,tz = "UTC"), "%d/%m/%Y") #date_decimal function is used to convert decimal into date format
print(jobs)
jobs$Month <- month(dmy(jobs$Date))  #month function is used to fetch the month from the date variable
month_correction <- function(d){     #User defined function is used to correct certain dates
  if(day(dmy(d))==31)
  {
    return (month(dmy(d)) + 1)
  }
  else 
  {
    return(month(dmy(d)))
  }
}
jobs$Month <- sapply(jobs$Date, month_correction) #sapply is used to apply month_correction function on each element
print(jobs$Month)

jobs$Total_Employment <- rowSums(jobs[,1:6], na.rm = T)           #Adding employment across all the states using rowSums
print(" Month with the highest total employment across the states ")
colMax <- function(data) sapply(data, max, na.rm = TRUE)
print(colMax(jobs[,-c(1:7)]))
Low_jobs_month <- jobs[(jobs$Atlantic < 950),]                    
print("Months with employement figure in Atlantic less than 950 ")
print(Low_jobs_month[ ,c(8,6)])
print("Sorted Data by Quebec")
sorted_jobs <- jobs[order(jobs$Quebec),]
print(sorted_jobs[,-9])