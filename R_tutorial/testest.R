library(dplyr)
library(tidyr)
HR <- read.csv("R/HR_comma_sep.csv")
summary(HR)
head(HR)

quantile(HR$satisfaction_level)
hist(HR$satisfaction_level)

HR_satisfact <- data.frame(HR$satisfaction_level, HR$last_evaluation)
head(HR_satisfact)

head(HR)
HR_long <- gather(HR, attribute, values, )
HR_long
head(HR_long)
tail(HR_long)
unique(HR_long)


