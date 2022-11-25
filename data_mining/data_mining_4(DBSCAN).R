#data preparation
library(dplyr)
library(dbscan)
library(ggplot2)
library(factoextra)

set.seed(2019)
synth.data2 <- data.frame(x1 = c(runif(50, 1, 5),
                                 rnorm(50, 1, 0.5),
                                 rnorm(50, 5, 1.5),
                                 rnorm(50, 8, 0.2)),
                          x2 = c(rnorm(50, 3, 0.2),
                                 rnorm(50, -1, 0.5),
                                 rnorm(50, 1, 0.3),
                                 runif(50, -1, 3)))
synth.data2 %>% ggplot(aes(x= x1, y= x2)) +
  geom_point(shape = 1)

synth.data2$cl <- rep(1:4, each = 50)
synth.data2 %>% ggplot(aes(x= x1, y= x2, col = factor(cl))) +
  geom_point(shape = 1)

#DBSCAN
dbclust <- dbscan(synth.data2, 1, minPts = 3)
fviz_cluster(dbclust, synth.data2, stand = F, frame = F, geom = "point")
