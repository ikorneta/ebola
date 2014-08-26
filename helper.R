# helper.R
# at this point in time, ShinyApps has a problem with dates in the %b format
# this is a workaround

helper <- function (date) 

{
  date <- gsub("Jan", "1", date)
  date <- gsub("Feb", "2", date)
  date <- gsub("Mar", "3", date)
  date <- gsub("Apr", "4", date)
  date <- gsub("May", "5", date)
  date <- gsub("Jun", "6", date)
  date <- gsub("Jul", "7", date)
  date <- gsub("Aug", "8", date)
  date <- gsub("Sep", "9", date)
  date <- gsub("Oct", "10", date)
  date <- gsub("Nov", "11", date)
  date <- gsub("Dec", "12", date)
    
  y <-  as.numeric(as.Date(date, "%d %m %Y"))- as.numeric(as.Date("2014-03-21"))
  return(y)
}







