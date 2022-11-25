#install package and set library
library(dygraphs)
library(ggplot2)
library(xts)

#data loading
economics <- ggplot2::economics
head(economics)

##set xts data for the time series
#실업률
eco.time.series <- xts(economics$unemploy, order.by = economics$date)
head(eco.time.series)

#make dygraph
dygraph(eco.time.series)

#set range selector
dygraph(eco.time.series) %>% dyRangeSelector()

##multivalue data setting
#저축률
eco_a <- xts(economics$psavert, order.by = economics$date)

#실업자 수
eco_b <- xts(economics$unemploy/1000, order.by = economics$date)

#data combine
econo2 <- cbind(eco_a, eco_b)
colnames(econo2) <- c("psavert", "unemploy")
head(econo2)

#visualization 
dygraph(econo2) %>% dyRangeSelector()
