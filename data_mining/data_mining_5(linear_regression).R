library(dplyr)
library(ggplot2)

exData <- read.csv("C:/Users/cjsso/Desktop/data mining/ex1data1.txt", header = F, col.names = c('pop', 'profit'))
ggplot(exData, aes(x = pop, y = profit)) +
  geom_point(shape = 4, col = 'red')

#initial setting
theta0 = 0
theta1 = 1

#hypothesis
h <- function(x, t0, t1) {
  t0 + t1 * x
} #선형 회귀식

ggplot(exData, aes(x = pop, y = profit)) +
  geom_point(shape = 4, col = 'red') +
  geom_abline(slope = theta1, intercept = theta0, col = 'blue') #y=x 그래프를 임의로 설정

#cost function
costJ <- function(t0, t1) {
  m = nrow(exData)
  1/(2*m) * sum( (h(exData$pop, t0, t1) - exData$profit) ** 2) #잔차의 평균
}

#cost of initial setting
costJ(theta0, theta1)

#cost function for various theta1
costDF <- data.frame(t1 = seq(-1, 2, 0.1),
                     cost = seq(-1, 2, 0.1) %>%
                       sapply(function(x) { costJ(theta0, x)})) #기울기(a)에 대한 cost function 값 
costDF %>% ggplot(aes(x = t1, y= cost)) + geom_point()

# find gradient
gradient_theta0 <- function(t0, t1){
  mean(h(exData$pop, t0, t1) - exData$profit) #theta0에 대한 기울기 값 계산
}
gradient_theta1 <- function(t0, t1){
  mean((h(exData$pop, t0, t1) - exData$profit) * exData$pop) #theta1에 대한 기울기 값 계산
}

gradient_theta0(theta0, theta1)
gradient_theta1(theta0, theta1)

#gradient descent
num_iter <- 1500
alpha <- 0.01 #learning rate

#initial setting
theta0 = 0
theta1 = 1
costDF = data.frame(iter = 0, t0 = theta0, t1 = theta1, cost = costJ(theta0,
                                                                     theta1))
# gradient descent
for(i in 1:num_iter) {
  theta0_update <- gradient_theta0(theta0, theta1)
  theta1_update <- gradient_theta1(theta0, theta1)
  #update
  theta0 <- theta0 - alpha * theta0_update
  theta1 <- theta1 - alpha * theta1_update
  costDF <- rbind(costDF, c(i, theta0, theta1, costJ(theta0, theta1)))
}
costDF[c(1:10, seq(100,1500, 100)), ]

#visualization
costDF %>% ggplot(aes(x = iter, y= cost)) + geom_point(alpha = 0.5)

ggplot(exData, aes(x = pop, y = profit)) + geom_point(shape = 4, col = 'red') +
  geom_abline(slope = theta1, intercept = theta0, col = 'blue') +
  ggtitle(sprintf("h*(x) = %.2f + %.2f*x cost = %.3f", round(theta0, 2),
                  round(theta1, 2), costJ(theta0, theta1)))

#contour
costVisDF <- data.frame(t0 = rep(seq(-12,4,0.8), each = 21), t1 = rep(seq(0,2,0.1), 21))
costVisDF$cost <- mapply(FUN = costJ, costVisDF$t0, costVisDF$t1)

ggplot(costVisDF, aes(t0, t1, z = cost)) + geom_raster(aes(fill = log(cost))) +
  geom_contour(col = 'white', binwidth = 3) + theme_bw() +
  scale_fill_gradient(low = 'red', high = 'white') +
  geom_point(data = costDF, aes(x = t0, y= t1), shape = 1, col = 'blue')

##exercise
#change the alpha
alpha <- 0.001 #smaller learning rate
alpha <- 0.05 #larger learning rate

alpha <- 0.00005

#initial setting
theta0 = 0
theta1 = 1
costDF = data.frame(iter = 0, t0 = theta0, t1 = theta1, cost = costJ(theta0, theta1))

#gradient descent
for(i in 1:num_iter) {
  theta0_update <- gradient_theta0(theta0, theta1)
  theta1_update <- gradient_theta1(theta0, theta1)
  #update
  theta0 <- theta0 - alpha * theta0_update
  theta1 <- theta1 - alpha * theta1_update
  costDF <- rbind(costDF, c(i, theta0, theta1, costJ(theta0, theta1)))
}
costDF[c(1:10, seq(100,1500, 100)), ]

#visualization
costDF %>% ggplot(aes(x = iter, y= cost)) + geom_point(alpha = 0.5)

ggplot(exData, aes(x = pop, y = profit)) + geom_point(shape = 4, col = 'red') +
  geom_abline(slope = theta1, intercept = theta0, col = 'blue') +
  ggtitle(sprintf("h*(x) = %.2f + %.2f*x cost = %.3f", round(theta0, 2),
                  round(theta1, 2), costJ(theta0, theta1)))

#contour
costVisDF <- data.frame(t0 = rep(seq(-12,4,0.8), each = 21), t1 = rep(seq(0,2,0.1), 21))
costVisDF$cost <- mapply(FUN = costJ, costVisDF$t0, costVisDF$t1)

ggplot(costVisDF, aes(t0, t1, z = cost)) + geom_raster(aes(fill = log(cost))) +
  geom_contour(col = 'white', binwidth = 3) + theme_bw() +
  scale_fill_gradient(low = 'red', high = 'white') +
  geom_point(data = costDF, aes(x = t0, y= t1), shape = 1, col = 'blue')
