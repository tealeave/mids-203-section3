library(tidyverse)
library(ggplot2)

x = seq(-3,3,.01)

plot(x,dnorm(x),col='black',type='l',lwd=2,ylab="Density_X")
lines(x,dt(x,5),type='l',col='red',lwd=2)
lines(x,dt(x,15),type='l',col='green',lwd=2)
lines(x,dt(x,30),type='l',col='blue',lwd=2)
legend('topleft',legend=c("Norm","df=5","df=15","df=30"),
       text.col=c('black','red','green','blue'),
       text.width = 0.5,bty='n')


#In a warehouse full of power packs labeled as 12 volts we randomly measured the 
# voltage of 7.  Here is the data:
  
voltage <- c(11.77, 11.90, 11.64, 11.84, 12.13, 11.99,  11.77)


#1. Find the mean and the standard deviation. 

sample_mean <- mean(voltage)
sample_sd   <- sd(voltage)

# The sample mean is: 
round(sample_mean, 2)

#the sample standard deviation is 
round(sample_sd, 2) 

# 2. Using `qt()`, compute the t critical value for a hypothesis test for this sample. 
#alpha = .05

t_critical <-  qt(1-.025, df = length(voltage)-1)


# The critical value is 
round(t_critical, 2)

#3. Define a test statistic, $t$, for testing whether the population mean is 12. 


t <- (sample_mean - 12) / (sample_sd / sqrt(length(voltage)))

#The test statistic that we calculate is 
round(t, 2) 


# 4. Calculate the p-value using the t statistic.

p <- 2 *  pt(t, df = length(voltage)-1)

# The p-value that we generate from this test is 
round(p, 4)


# This isn't necessary, but each of the steps that we've taken above 
# could (and in fact probably should) be written into a function. Of course, 
# there is also a built-in function `t.test`, but you can compose this with 
# the pieces as well. 

# r define our own t_test function

# t_test <- function(data, null_hypothesis, alpha) { 
#   ## data is the input data
#   ## null_hypothesis what we are testing against 
#   
#   sample_mean <- mean(data) 
#   sample_sd   <- sd(data) 
#   standard_error <- sample_sd / sqrt(length(data))
#   
#   critical_value <- qt(alpha/2, df = length(data) - 1)
#   
#   test_statistic <- (sample_mean - null_hypothesis) / standard_error
#   
#   ## to catch both tails of the test, multiply by 2
#   p_value <- pt(test_statistic, df = length(data) - 1) * 2 
#   
#   test_result <- list( 
#     'sample_mean'    = sample_mean, 
#     'sample_sd'      = sample_sd, 
#     'standard error' = standard_error, 
#     'critical_value' = critical_value,
#     'test_statistic' = test_statistic, 
#     'p_value'        = p_value
#   )
#   
#   return(test_result)
#   
# }


# test_result <- t_test(data = voltage, null_hypothesis = 12, alpha = 0.05)
# test_result
# 
# #And so, we see that the sample average was `
# round(test_result$sample_mean, 2)
# 
# #and that the p-value for the test is 
# round(test_result$p_value, 4)

#5. Should you reject the null?  Argue this in two different ways.

# No.  You can see this in two ways.  First, the t-statistic is less, 
# in absolute value, than the critical value.  Second, the p-value is above 0.05. 
# These are equivalent information.

# 6. Suppose you were to use a normal distribution instead of a t-distribution 
# to test your hypothesis.  What would your p-value be for the z-test?
  

p_normal <- 2 * pnorm(t)

# If we compare this test statistic against a normal distribution, 
# then we would generate a p-value that is 

round(p_normal, 4)

# Notice that we would have made the wrong decision here.

# 7. Without actually computing it, say whether a 95% confidence interval for the 
# mean would include 12 volts.

# Because the null of 12 volts is not rejected, we automatically know the 
# confidence interval will include 12 volts.
# 8. Compute a 95% confidence interval for the mean.

#test_result

#test_result$sample_mean + c(1,-1)*test_result$critical_value*(test_result$sample_sd/(length(voltage))^0.5)

sample_mean +c(-1,+1)*t_critical*(sample_sd/length(voltage)^0.5)


# # 8. Compute a 95% confidence interval for the mean.
# 
# # Here, we'll write an add-on for the function that we used earlier. 
# # This is going to expect an object that is shaped like like the object created by 
# #`t_test`. 
# 
# 
# 
# confidence_interval <- function(test_object) { 
#   require(magrittr, quietly = TRUE) # to give access to pipes like %>% and, in particular %$% 
#   
#   lower <- test_object %$% (sample_mean - (critical_value * test_statistic))
#   upper <- test_object %$% (sample_mean + (critical_value * test_statistic)) 
#   
#   ## equivalently, each of these could be: 
#   ## lower <- test_object$sample_mean - test_object$critical_value * test_object$test_statistic
#   
#   ci <- list(
#     'lower' = lower, 
#     'upper' = upper
#   )
#   
#   return(ci)
# }
# 
# ci_ <- confidence_interval(test_result)
# 
# # And so, the 95% confidence interval for this data is 
# round(c(ci_$lower, ci_$upper), 4)`]






# Data Exercise


## t-Test Micro Cheat Sheet
# - Key t-Test Assumptions
# - Metric variable
# - IID
# - No major deviations from normality, considering sample size

## Testing the Home Team Advantage

# The file athlet2.Rdata contains data on college football games.  
# The data is provided by Wooldridge and was collected by Paul Anderson, 
# an MSU economics major, for a term project.  Football records and scores are 
# from 1993 football season.

setwd("C:\\Berkeley\\DataSci203_Spring_2025\\temp203\\2025_spring\\live_session\\unit_06")

load("athlet2.RData")

colnames(data)
head(data)

# We are especially interested in the variable, dscore, which represents the score 
# differential, home team score - visiting team score.  
# We would like to test whether a home team really has an advantage over the 
# visiting team.
#
#> We begin by examining the dscore variable

# There are 
nrow(data) #observations in the dataframe, and 
sum(is.na(data$dscore)) #'NAs` in the varible we care about dscore
# That's good news so far. 



data %>% 
  ggplot() + 
  aes(x = dscore) + 
  geom_histogram(bins = 10) + 
  theme_minimal() + 
  labs(
    title = 'Distribution of Home vs. Away Score Differential',
    subtitle = 'Measured in `dscore` variable', 
    x = 'Home vs. Away Score Differential'
  )


# There's a clear central tendency, and the dispersion is reasonably contained. 
# As well, because we've got 
sum(!is.na(data$dscore)) #observations, we just barely meet the rule-of-thumb 
#requirement for the CLT. 

# In this scenario, you could try to argue that 30 observations might not really 
# be enough to achieve a normal sampling distribution.  
# However, the histogram of `dscore` doesn't reveal any substantial skew, which is 
# when we would normally worry about that we need more than 30 observations, 
# so this argument isn't very strong.

# For students arguing that the t-test is invalid, a better approach might 
# be to ask whether the data points are truly independent.  
# There are only so many college football teams, and it seems likely that 
# each team appears in multiple rows of data.  
# If a team achieves a high score in one game, it is probably likely to achieve a 
# high score in other games.  
# This suggests that we may be seeing less variation in our data set than 
# actually exists in the population, and the denominator in our t-statistic 
# will be too small, exaggerating our results.

# 2. Should you perform a one-tailed test or a two-tailed test?  What is the strongest argument for your answer?
  
  # Even though you are specifically interested in a home team *advantage*, 
  # the better answer is to choose the two-tailed test.  
  # There are several reasons for this:
  
  # Your reader may not share your theory that home teams have the advantage 
  # and may even believe the opposite.

  # The one-tailed test makes it easier to reject the null, 
  # but there is no way to prove to your reader that you didn't start with a 
  # two-tailed test and then switch to the one-tail once you saw the direction 
  # of the effect.  That would be cheating and your type-1 error rate would be 
  # greater than .05.  As a result, readers may be suspicious of whether 
  # you really committed to the type of test ahead of time.

  # Imagine that you select the one-tailed test, but then the data surprises 
  # you and you find that the visiting team has a strong advantage.  
  # Suppose the visiting team actually has a 10 point advantage over the home team.  By the rules of hypothesis testing, you can't reject the null, no matter how big the visiting team advantage is.  But would you really pack up your study at this point and say you were unable to find evidence against the null?  Will your reader believe that you'd really do this, and that you wouldn't cheat by switching to the two-tailed test?
  
  # 3. Execute the t-test and interpret every component of the output.


t.test(data$dscore, mu=0)
?t.test


# 4. Based on your output, suggest a different hypothesis that would have led to 
# a different test result.  
# Try executing the test to confirm that you are correct.

# The easiest thing to do is to look at the 95% confidence interval.  
# Since 0 is inside the interval, we already know that the null cannot be rejected.  
# If we had chosen a null hypothesis outside of this interval, say $mu=7$, it would have been rejected.


t.test(data$dscore, mu = 7)

## t_test(data$dscore, null_hypothesis = 7, alpha = 0.05) 
# Good news, our result matches the built-in still!

#  Assumptions Behind the t-test

# For the following scenarios, what is the strongest argument against the validity of a t-test?
  
#  - You have a sample of 50 CEO salaries, and you want to know whether 
#   the mean salary is greater than $1 million.

# - A nonprofit organization measures the percentage of students that pass an 8th grade 
# reading test in 40 neighboring California counties.  
# You are interested in whether the percentage of students that pass in California 
# is over 80%

#- You have survey data in which respondents assess their own opinion of corgis, 
# with options ranging from "1 - extreme disgust" to "5 - affection so intense it threatens 
# my career."  You want to know whether people on the average like corgis more than 3, 
# representing neutrality.