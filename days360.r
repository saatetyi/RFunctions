# Function logic based on Excel Days360 function described here:
#   http://office.microsoft.com/en-us/excel-help/days360-function-HP010062286.aspx
# Author: Peter Szabo, szabo (dot) peter (at) sch (dot) bme (dot) hu
# Github: saatetyi
# Version: v.0.1

days360<-function(start_date, end_date, method=T){
  # Init ...
  diffDays<-0
  startYear<-as.numeric(format(start_date, "%Y"))
  startMonth<-as.numeric(format(start_date, "%m"))
  startDay<-as.numeric(format(start_date, "%d"))
  endYear<-as.numeric(format(end_date, "%Y"))
  endMonth<-as.numeric(format(end_date, "%m"))
  endDay<-as.numeric(format(end_date, "%d"))
  
  if (method){ # European method
    if (startDay>30){
      startDay<-30
    }
    
    if (endDay>30){
      endDay<-30
    }
  } else { # U.S. method
    if (format(start_date, "%m")!=format(start_date+1, "%m")){ # last day of the month
      startDay<-30
    }
    if (format(end_date, "%m")!=format(end_date+1, "%m")) { # end date is the last day of the month 
      #and the start dy is earlyer than the last day of a month
      if (startDay < 30) {
        # the ending date becomes equal to the 1st day of the next month;
        endDay<-1
        endMonth<-endMonth+1
      } else {
        #otherwise the ending date becomes equal to the 30th day of the same month
        endDay<-30
      }
    }
  }
  # Init finished
  
  # Calculation starts ...
  if (startDay<=endDay) {
    diffDays<-endDay-startDay
  } else {
    diffDays<-30-startDay+endDay
    endMonth<-endMonth-1
  }
  
  if (startMonth<=endMonth){
    diffDays<-diffDays+(endMonth-startMonth)*30
  } else {
    diffDays<-diffDays+(12-startMonth+endMonth)*30
    endYear<-endYear-1
  }
  
  if (startYear<=endYear){
      diffDays<-diffDays+(endYear-startYear)*12*30
  }
  # Calculation ended
  
  return(diffDays)
}
