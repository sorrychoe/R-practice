library(ggiraphExtra)
library(tibble)
library(ggplot2)

str(USArrests)
crime <- rownames_to_column(USArrests, var = "state")
crime$state <- tolower(crime$state)

states_map <- map_data("state")
str(states_map)

ggChoropleth(data = crime,
             aes(fill = Murder,
                 map_id = state),
             map = states_map,
             interactive = T)
