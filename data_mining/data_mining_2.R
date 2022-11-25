library(tidyr)
library(dplyr)

#chapter 1: tidyr review
load(url('https://github.com/hbchoi/SampleData/raw/master/weather.RData'))

#exercise 1
weather2.temp <- weather2 %>%
  filter(measure %in% c("Max.TemperatureF", "Mean.TemperatureF", "Min.TemperatureF")) %>%
  group_by(measure) %>%
  mutate(monthlymean = mean(day01:day31, na.rm = T)) %>%
  gather(key, value, -year, -month, -measure) %>%
  filter(key %in% "monthlymean")
head(weather2.temp)

#chapter 2: information visualization with ggplot
library(ggplot2)
str(mtcars)

ggplot(mtcars, aes(x = cyl, y = mpg)) +
  geom_point()

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) + #for categorical value(cyl)
  geom_point()

#ggplot2 layer: aesthetics & geometries
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6)

#ggplot2 layer: facets
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species)

#ggplot2 layer: statistics
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) +
  stat_smooth(method = "lm", se = F, col = 'red')

#ggplot2 layer: coordinates
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) +
  stat_smooth(method = "lm", se = F, col = 'red') +
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)",
                     limits = c(4,8),
                     expand = c(0,0)) +
  coord_equal()

#ggplot2 layer: themes
levels(iris$Species) <- c("Setosa", "Versicolor", "virginica")
library(grid)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ Species) +
  stat_smooth(method = "lm", se = F, col = 'red') +
  scale_y_continuous("Sepal Width (cm)",
                     limits = c(2,5),
                     expand = c(0,0)) +
  scale_x_continuous("Sepal Length (cm)",
                     limits = c(4,8),
                     expand = c(0,0)) +
  coord_equal() + 
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        strip.background = element_blank(),
        axis.text = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.text = element_blank(),
        panel.margin = unit(1, "line")
        )
