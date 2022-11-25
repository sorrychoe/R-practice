library(dplyr)
library(ggplot2)

#set the data
set.seed(2022)
synth.data <- data.frame(x1 = c(rnorm(20, 3, 1.5), rnorm(20,0,1), rnorm(20,5,1)),
                         x2 = c(rnorm(20, 0, 1), rnorm(20,4,1), rnorm(20,5,1))) #난수 데이터 형성
ndata <- nrow(synth.data) #열의 수
ndim <- ncol(synth.data) #행의 수
synth.data %>% ggplot(aes(x = x1, y= x2)) + geom_point(shape = 1) + theme_bw() #visualization

u_dist <- function(u, v){
  sqrt(sum((u - v) ** 2))
}

#set k value
k <- 3
cents <- data.frame(cl = 1:k)
cents <- cbind(cents, synth.data[sample(1:60, k),])

#initial setting
synth.data$cl <- factor(rep(1, ndata), levels = 1:k)
synth.data %>% ggplot(aes(x = x1, y= x2, col = cl)) + geom_point(shape = 1) + theme_bw() +
  geom_point(data = cents, shape = 4, col = 'red') 

#Assign Data Points to their Closest Centroid
label <- array(0,ndata)
cents.point <- array(unlist(cents), 
                     c(3,3))
cents.point <- cents.point[ ,-1]
synth.point <- array(unlist(synth.data),
                     c(60,3))
synth.point <- synth.point[,-3]

for(i in 1:ndata){
  distance = array(0,k)
  for(j in 1:k){
    distance[j] <- u_dist(synth.point[i,], cents.point[j,])
  }
  cluster = which(distance == min(distance))
  label[i] = cluster
}

for (s in 1:ndata){
  synth.data[s,3] = label[s]
}

synth.data %>% ggplot(aes(x = x1, y= x2, col = cl)) + geom_point(shape = 1) + theme_bw() +
  geom_point(data = cents, shape = 4, col = 'red')

#Move Centroids to Clusters’ Average Point
centroids.1 <- synth.data %>%
  filter(synth.data$cl == 1) %>%
  summarise(x1 = mean(x1), x2 = mean(x2), cl = 1)

centroids.2 <- synth.data %>%
  filter(synth.data$cl == 2) %>%
  summarise(x1 = mean(x1), x2 = mean(x2), cl = 2)

centroids.3 <- synth.data %>%
  filter(synth.data$cl == 3) %>%
  summarise(x1 = mean(x1), x2 = mean(x2), cl = 3)

cents <- rbind(centroids.1, centroids.2)
cents <- rbind(cents, centroids.3)

synth.data %>% ggplot(aes(x = x1, y= x2, col = cl)) + geom_point(shape = 1) + theme_bw() +
  geom_point(data = cents, shape = 4, col = 'red')

###Repeat Until No Change
#Assign Data Points to their Closest Centroid
label <- array(0,ndata)
cents.point <- array(unlist(cents), 
                     c(3,3))
cents.point <- cents.point[ ,-3]
synth.point <- array(unlist(synth.data),
                     c(60,3))
synth.point <- synth.point[,-3]

for(i in 1:ndata){
  distance = array(0,k)
  for(j in 1:k){
    distance[j] <- u_dist(synth.point[i,], cents.point[j,])
  }
  cluster = which(distance == min(distance))
  label[i] = cluster
}

for (s in 1:ndata){
  synth.data[s,3] = label[s]
}

#Move Centroids to Clusters’ Average Point
centroids.1 <- synth.data %>%
  filter(synth.data$cl == 1) %>%
  summarise(x1 = mean(x1), x2 = mean(x2), cl = 1)

centroids.2 <- synth.data %>%
  filter(synth.data$cl == 2) %>%
  summarise(x1 = mean(x1), x2 = mean(x2), cl = 2)

centroids.3 <- synth.data %>%
  filter(synth.data$cl == 3) %>%
  summarise(x1 = mean(x1), x2 = mean(x2), cl = 3)

cents <- rbind(centroids.1, centroids.2)
cents <- rbind(cents, centroids.3)

synth.data %>% ggplot(aes(x = x1, y= x2, col = cl)) + geom_point(shape = 1) + theme_bw() +
  geom_point(data = cents, shape = 4, col = 'red')


#Total Within Sum of Square
sqrt.edist <- function(u, v){
  sum((u - v) ** 2)
}
withinss<-
  sapply(1:k, function(i) {
    sum(
      apply(split(synth.data, synth.data$cl) [[i]] [, 1:ndim], 1, function(x) {
        sqrt.edist(x, cents[i,-1])
      })
    )
  })
sum(withinss)

#Calinski-Harabasz index(CH index)
wss <- sum(withinss)
all.center <- colMeans(synth.data[ ,1:ndim])
tss <- sum(
  apply(synth.data[, 1:ndim], 1, function(x) { sqrt.edist(x, all.center)}))
bss <- tss - wss
ch.index <- (bss/(k-1)) / (wss/(ndata-k))
ch.index
