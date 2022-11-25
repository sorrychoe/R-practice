library(httr)
library(tidyverse)
library(rvest)
library(tidytext)


url <- 'https://trees.gamemeca.com/portfolio-item/gamerank202208-4/#1521881342690-f60aa3c1-8642'

game <- get(url)


xpath <- '//*[@id="tablepress-746"]/tbody'
