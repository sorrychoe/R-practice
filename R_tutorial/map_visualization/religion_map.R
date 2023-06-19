#library settng
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(textdata)
library(glue)
library(reshape2)
library(readxl)

#load data
gunsan = read_excel("C:\\Users\\cjsso\\Downloads\\전라북도 군산시_종교단체현황_20210825.xlsx")

#data check
head(gunsan)

#data preparation
chris.gunsan <- subset(gunsan, 종교분류 == '기독교', select = c(종교단체명, 소재지지번주소))

chris.gunsan %>% unnest_tokens(location, 소재지지번주소) -> locate

locate <- locate[!(locate$location == "전라북도"),]
locate <- locate[!(locate$location == "군산시"),]
locate <- locate[!duplicated(locate[,1]),]

#word count
locate %>%
  group_by(location) %>%
  count(location) %>%
  arrange(desc(n)) -> count_loc

#frequency visualization
count_loc %>%
  head(20) %>%
  ggplot(aes(x = n, y = reorder(location, n), fill = n)) +
  geom_bar(stat='identity')

#wordcloud
wordcloud2(count_loc)


#map visualization
library(ggmap)
ggmap_key <- 'AIzaSyB10NsX0eU4zoQ2Op1fPf8UbVbOeMRuecE'
register_google(ggmap_key)
gunsan_data <- mutate_geocode(data = gunsan, location = 소재지도로명주소, source = 'google')

head(gunsan_data)
gunsan_data <- gunsan_data[1:659,]

gunsan_loc <-left_join(gunsan_data, locate)
#gunsan_loc_count <- left_join(gunsan_loc, count_loc)


gunsan_map <- get_googlemap('군산', maptype = 'satellite', zoom = 12)
ggmap(gunsan_map) +
  geom_point(data = gunsan_loc, aes(x = lon, y = lat, color = factor(location)), size = 1)


naun_church <- gunsan_loc[(gunsan_loc$location == '나운동'),] %>% filter(!is.na(location))

data_marker <- data.frame(naun_church$lon, naun_church$lat)
gunsan_map_naun <- get_googlemap('나운동', maptype = 'roadmap', zoom = 15, markers = data_marker)
ggmap(gunsan_map_naun) +
  geom_text(data = naun_church, aes(x = lon, y = lat),  size = 2.5, label = naun_church$종교단체명)
