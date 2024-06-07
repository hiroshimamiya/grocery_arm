### code to create time varialbes in 2016


#New Year's Day	Fri, Jan 1, 2016
#Good Friday	Fri, Mar 25, 2016
#Easter Monday	Mon, Mar 28, 2016
#National Patriots' Day	Mon, May 23, 2016
#Nativity of Saint John the Baptist	Fri, Jun 24, 2016
#Canada Day	Fri, Jul 1, 2016
#Labour Day	Mon, Sept 5, 2016
#Thanksgiving	Mon, Oct 10, 2016
#Christmas Day	Mon, Dec 26, 2016

library(dplyr)
calender <-  seq(as.Date("2015/1/1"), as.Date("2017/12/31"), "days") %>% 
  as_tibble() %>% 
  dplyr::rename(date = value) %>% 
  dplyr::mutate(holiday = 0) 
calender$holiday[calender$date == "2016/1/1"] <- 1 
calender$holiday[calender$date == "2016/3/28"] <- 1  
calender$holiday[calender$date == "2016/5/23"] <- 1  
calender$holiday[calender$date == "2016/6/24"] <- 1
calender$holiday[calender$date == "2016/7/1"] <- 1  
calender$holiday[calender$date == "2016/9/5"] <- 1  
calender$holiday[calender$date == "2016/10/10"] <- 1  
calender$holiday[calender$date == "2016/12/25"] <- 1  

calender$holiday[calender$date == "2017/1/1"] <- 1  
calender$holiday[calender$date == "2017/4/17"] <- 1  
calender$holiday[calender$date == "2017/5/22"] <- 1  
calender$holiday[calender$date == "2017/6/24"] <- 1
calender$holiday[calender$date == "2017/7/1"] <- 1  
calender$holiday[calender$date == "2017/9/4"] <- 1  
calender$holiday[calender$date == "2017/10/9"] <- 1  
calender$holiday[calender$date == "2017/12/25"] <- 1  

#calender$holiday[calender$date == "2015/1/1"] <- 1  
calender$holiday[calender$date == "2015/4/3"] <- 1  
calender$holiday[calender$date == "2015/5/18"] <- 1  
calender$holiday[calender$date == "2015/6/24"] <- 1
calender$holiday[calender$date == "2015/7/1"] <- 1  
calender$holiday[calender$date == "2015/9/7"] <- 1  
calender$holiday[calender$date == "2015/10/12"] <- 1  
calender$holiday[calender$date == "2015/12/25"] <- 1  

calender$holidayInd <- 0 
for(i in 1:7) calender[which(calender$holiday == 1)-i, "holidayInd"] <- 1


### halloween
calender$hall <- 0
calender$hall[calender$date == "2015/10/31"] <- 1  
calender$hall[calender$date == "2016/10/31"] <- 1  
calender$hall[calender$date == "2017/10/31"] <- 1  
calender$hallInd <- 0 

for(i in 1:14) calender[which(calender$hall == 1)-i, "hallInd"] <- 1
calender %>%  filter(date >  "2015-10-29" & date <  "2015-11-10")


### post holiday indicator 
calender$postHoliday <- 0
calender$postHoliday[calender$date == "2016/1/3"] <- 1 
calender$postHoliday[calender$date == "2017/1/3"] <- 1  

calender$postHolidayInd  <- 0
for(i in 1:-7) calender[which(calender$postHoliday == 1)-i, "postHolidayInd"] <- 1
calender %>% filter(postHolidayInd == 1)

for(i in 1:-7) calender[which(calender$hall == 1)-i, "postHolidayInd"] <- 1
calender %>%  filter(date >  "2015-10-20" & date <  "2015-11-10")




# cpi inflation adjustment
### >>> CPI -----
cpi.adj <- read.csv("data/cpi_monthly_adj.csv", header=T, col.names=c("geo", "ref", "food"))
library(zoo)
cpi.adj$date <- as.yearmon(cpi.adj$ref, "%B %Y" )
saveRDS(calender, "data_hiroshi/holiday.rds")
