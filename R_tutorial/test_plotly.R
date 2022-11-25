library(plotly)
library(ggplot2)

p <- ggplot(data = mpg, aes(x = displ, y = hwy, col = drv)) +
  geom_point()

ggplotly(p)

s <- ggplot(data = diamonds, aes(x = cut, col = clarity)) +
  geom_bar(position = "dodge")

ggplotly(s)
