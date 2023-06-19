unemployment <- read.csv('https://github.com/hbchoi/SampleData/raw/master/unemployment.csv')

#set alpha = 1
alpha <- 1
unemployment$est_y <- alpha * unemployment$male_unemployment
unemployment$error <- unemployment$female_unemployment - unemployment$est_y
unemployment

#mean of squared error
mse = mean(unemployment$error ** 2)
mse

#simplified example
plot(x = unemployment$male_unemployment,
     y = unemployment$female_unemployment,
     main = 'simple example',
     xlab = 'male unemployment rate %',
     ylab = 'fmale unemployment rate %',
     xlim = c(0,10), ylim = c(0,10))
abline(0, alpha, col = 'red')
text(x = 2, y = 8, 'y = x', col = 'red')

##try different model
#set alpha = 0.9
alpha <- 0.9
unemployment$est_y <- alpha * unemployment$male_unemployment
unemployment$error <- unemployment$female_unemployment - unemployment$est_y
unemployment


#mean of squared error
mse = mean(unemployment$error ** 2)
mse

#findMSE function
findMSE <- function(alpha){
  mse <- mean((unemployment$female_unemployment - 
                 unemployment$male_unemployment * alpha) ** 2)
}

alpha_list <- seq(0.5, 1.5, 0.01)
MSE_list <- sapply(alpha_list, findMSE)
plot(x = alpha_list, y = MSE_list, xlab = 'alpha', ylab = 'MSE')
best_alpha <- alpha_list[which.min(MSE_list)]
best_alpha

#simplified example that best alpha
plot(x = unemployment$male_unemployment,
     y = unemployment$female_unemployment,
     main = 'simple example',
     xlab = 'male unemployment rate %',
     ylab = 'fmale unemployment rate %',
     xlim = c(0,10), ylim = c(0,10))
abline(0, best_alpha, col = 'red')
text(x = 2, y = 8, 'y = 0.92*x', col = 'red')

##optimizing model
alpha <- 0.6945
beta <- 1.4341
unemployment$est_y <- alpha * unemployment$male_unemployment + beta
unemployment$error <- unemployment$female_unemployment - unemployment$est_y
unemployment

#mean of squared error
mse = mean(unemployment$error ** 2)
mse

#visualization
plot(x=unemployment$male_unemployment,
     y=unemployment$female_unemployment,
     main = 'simple example',
     xlab = 'male unemployment rate %',
     ylab = 'fmale unemployment rate %',
     xlim = c(0,10), ylim = c(0,10))
abline(1.4341, 0.6945, col = 'red')
text(x= 2, y= 8, 'y = 0.6945*x + 1.4341', col =
       'red')

#find the model minimizing MSE
fmla <- female_unemployment ~ male_unemployment
unemployment_model <- lm(fmla, data = unemployment)
unemployment_model
