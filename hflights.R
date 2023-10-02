#install.packages("hflights")
setwd("~/data-analytics-tot")
library("hflights")

library(dplyr)


dresult <-  hflights %>%
            filter(Origin == "IAH")%>%
            mutate(FDelayed = if_else(DepDelay>0,TRUE,FALSE,missing=NULL))%>%
            group_by(UniqueCarrier)%>%
            summarise(No=n(),NumDelayed=sum(FDelayed,na.rm=TRUE))%>%
            mutate(DPH=100*(NumDelayed/No))%>%
            arrange(desc(DPH))
###filter
d3 <- hflights %>% filter(Origin == "IAH" 
                          & DepDelay>0 
                          & UniqueCarrier=="UA")

df<- as.data.frame(hflights)

###Select

d1 <- select(hflights,
             CarrierName = UniqueCarrier, 
             DepDelay,ArrDelay)

d2 <-select(hflights,
            CarrierName = UniqueCarrier, 
            contains("Delay"))

d3 <- select(hflights,1:3)

###using pipe function  %>% 
### ctrl-shift-m

d1 <- hflights %>% select(CarrierName = UniqueCarrier, FlightNum, DepDelay,ArrDelay)

d2 <- hflights %>% select(CarrierName = UniqueCarrier,contains("Delay"))

d3 <- hflights %>% select(1:3)

t <- mtcars %>%
  summarise( n = n())

 plot(hflights$,hflights$AirTime)

