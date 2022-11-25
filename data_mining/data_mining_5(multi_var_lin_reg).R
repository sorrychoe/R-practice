exData2 <- read.csv("C:/Users/cjsso/Desktop/data mining/ex1data2.txt", header = F, col.names = c('size', 'num_bedroom','price'))

#aggrigate
mean1 <- mean(exData2$size)
sd1 <- sd(exData2$size)
mean2 <- mean(exData2$num_bedroom)
sd2 <- sd(exData2$num_bedroom)

#normalization
exData2$size.norm <- (exData2$size - mean1) / sd1
exData2$num_bedroom.norm <- (exData2$num_bedroom - mean2) / sd2

featureDF <- exData2 %>% #normalized data frame
  mutate(bias = 1) %>%
  select(bias, ends_with('norm'))

labelVector <- exData2$price

#theta vector
theta_vector <- c(0, 0, 0)

#hypothesis
h <- function(x, theta_vector) {
  # x is feature vector
  theta_vector %*% x ## inner product of two vectors
}

# cost function
costFun <- function(theta_vector) {
  v <- as.matrix(featureDF) %*% theta_vector - labelVector(t(v) %*% v / (2*nrow(featureDF)))[1,1]
}

costFun(theta_vector)

costDF <- data.frame(iter = 0,
                     t0 = theta_vector[1],
                     t1 = theta_vector[2],
                     t2 = theta_vector[3],
                     cost = costFun(theta_vector))

## find optimal theta
num_iter <- 1500
alpha <- 0.05
n <- nrow(featureDF)

#gradient descent
for(i in 1:num_iter){
  theta_update <- h(, theta_vector) * alpha
  theta_vector <- theta_vector - theta_update
  costDF <- rbind(costDF, c(i, theta_vector, costFun(theta_vector)))
}

