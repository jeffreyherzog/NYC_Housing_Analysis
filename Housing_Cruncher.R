###################################
# Let's do some housing analysis!
###################################

library(tidyverse)
library(lubridate)
library(stringr)

####################### preliminaries #########################
#Data is from http://www1.nyc.gov/site/finance/taxes/property-annualized-sales-update.page
#I haven't set up an automatic ftp yet. Also the file names have to be made universal to XXXX_brooklyn.csv

setwd("C:/Users/Jeff/Documents/Housing")

############## Read in the data, downloaded manually. Let's start with Brooklyn #######################
#note: had to save them as csv's

#years <- c(2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
years <- c(2011,2012,2013,2014,2015,2016)

complete.set <- data.frame()

for (i in 1:length(years)) {
  if (i==1) {
    data <- read.csv(paste0(years[i],"_brooklyn.csv"),skip = 4,stringsAsFactors = FALSE)
    names <- colnames(data)
    data$YEAR <- years[i]
  }
  if (i>1) {
    data <- read.csv(paste0(years[i],"_brooklyn.csv"),skip = 4,stringsAsFactors = FALSE,col.names = names)
    data$YEAR <- years[i]
  }
  complete.set <- rbind(complete.set,data)
}

complete.set$NEIGHBORHOOD <-trimws(complete.set$NEIGHBORHOOD)
complete.set$SALE.PRICE<- gsub(",","",complete.set$SALE.PRICE)
complete.set$SALE.PRICE<- gsub("[$]","",complete.set$SALE.PRICE)
complete.set$SALE.PRICE<- as.numeric(complete.set$SALE.PRICE)
complete.set$BUILDING.CLASS.AT.TIME.OF.SALE<-trimws(complete.set$BUILDING.CLASS.AT.TIME.OF.SALE)

neighborhood.names <- unique(complete.set$NEIGHBORHOOD)

complete.set %>% filter(NEIGHBORHOOD=="BROOKLYN HEIGHTS",SALE.PRICE!=0,TAX.CLASS.AT.TIME.OF.SALE<3) %>%
  group_by(YEAR) %>% summarize(AVEPRICE = mean(SALE.PRICE)) 

#some are sales of huge buildings
t(as.data.frame(spread(complete.set %>% filter(NEIGHBORHOOD=="BROOKLYN HEIGHTS",SALE.PRICE!=0,TAX.CLASS.AT.TIME.OF.SALE<3) %>%
                         group_by(YEAR,BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
                         summarize(AVEPRICE = mean(SALE.PRICE)),key ="BUILDING.CLASS.AT.TIME.OF.SALE",value = "AVEPRICE")))

#tax class 2 is condos and coops.
j = 22 #17 is cobble hill, 9 is BH, 22 is dumbo?
for (i in 1:2) {
print(paste0("Annual average home price of all housing classes in ",neighborhood.names[j]))
print(complete.set %>% filter(NEIGHBORHOOD=="BROOKLYN HEIGHTS",SALE.PRICE!=0,TAX.CLASS.AT.TIME.OF.SALE==i) %>%
  group_by(YEAR) %>% summarize(AVEPRICE = mean(SALE.PRICE),n=n()))
print(paste0("Average price exlcuding stuff less than 10mn in ",neighborhood.names[j]))
print(t(as.data.frame(spread(complete.set %>% filter(NEIGHBORHOOD=="BROOKLYN HEIGHTS",SALE.PRICE<10000000,TAX.CLASS.AT.TIME.OF.SALE==i) %>%
                         group_by(YEAR,BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
                         summarize(AVEPRICE = mean(SALE.PRICE)),key ="BUILDING.CLASS.AT.TIME.OF.SALE",value = "AVEPRICE"))))
print(paste0("Number of sales exlcuding stuff less than 10mn in ",neighborhood.names[j]))
print(t(as.data.frame(spread(complete.set %>% filter(NEIGHBORHOOD=="BROOKLYN HEIGHTS",SALE.PRICE<10000000,TAX.CLASS.AT.TIME.OF.SALE==i) %>%
                         group_by(YEAR,BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
                         summarize(SALES = n()),key ="BUILDING.CLASS.AT.TIME.OF.SALE",value = "SALES"))))
print(paste0("Average age when built exlcuding stuff less than 10mn in ",neighborhood.names[j]))
print(t(as.data.frame(spread(complete.set %>% filter(NEIGHBORHOOD=="BROOKLYN HEIGHTS",SALE.PRICE<10000000,TAX.CLASS.AT.TIME.OF.SALE==i) %>%
                               group_by(YEAR,BUILDING.CLASS.AT.TIME.OF.SALE) %>% 
                               summarize(AGE = round(mean(YEAR.BUILT),0)),key ="BUILDING.CLASS.AT.TIME.OF.SALE",value = "AGE"))))
}



str(complete.set)  
  






