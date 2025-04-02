# "Unit 9 Live Session: Large-Sample Regression Theory (OLS Inference)

library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2)
library(regclass)
library(moments)
library(stats)
library(car)

#dev.off()

theme_set(theme_minimal())


# Model A ~ U(-0.5,0.5)

#various impacts on the error structure or epsillon

rA <- function(n){
  x = runif(n, min=-1, max = 1)
  # x and y are unrelated
  epsilon = runif(n, min=-.5, max=.5)
  y= 0 + epsilon
  return( data.frame(x=x,y=y) )
}


# B ~ Uniform on (- |X|, |X| )
# note dependent on X absolute value
# epsilon gets tighter  range as x gets closer to 0
rB <- function(n){
  x = runif(n, min=-1, max = 1)
  # take 100 from the set
  # runif will generate random numbers for each pair of minimum and maximum values.
  epsilon = runif(n, min=- abs(x), max=abs(x))
  y= 0 + epsilon
  return( data.frame(x=x,y=y) )
}



# C ~ U(-1 + |X|, 1- |X|)
# Epsilon expands out  as X gets closer to 0

rC <- function(n){
  x = runif(n, min=-1, max = 1)
  epsilon = runif(n, min= -1 + abs(x), max=1 - abs(x))
  y= 0 + epsilon
  return( data.frame(x=x,y=y) )
}

# limited to range smaller values  (more neg values) as gets X gets closer to 0 
# reach maxs as get closer to tails

rD <- function(n){
  x = runif(n, min=-1, max = 1)
  epsilon = runif(n, min=-1+abs(x), max=abs(x))
  y= 0 + epsilon
  return( data.frame(x=x,y=y) )
}

rE <- function(n){
  min=-1
  max=1
  #range
  rng = max - min
  x = runif(n, min=min, max=max)
  #cuts down on range x and epsillon as X gets closer to min
  # opens up as x gets larger and approaches max
  epsilon = runif(n, min= -((x-min)/rng), max=(x-min)/rng)
  y= 0 + epsilon
  return( data.frame(x=x,y=y) )}


data <- rbind( data.frame( rA(200), label = 'A'),
               data.frame( rB(200), label = 'B'),
               data.frame( rC(200), label = 'C'),
               data.frame( rD(200), label = 'D'),
               data.frame( rE(200), label = 'E')            
)
data %>% ggplot(aes(x=x, y=y)) + geom_point() + xlim(-2,2) + ylim(-1,1) + 
  facet_grid(rows=vars(label)) + ggtitle('Samples Drawn from Five Distributions')


data = rA(10)

#Variance or residuals does not look impacted

data %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method='lm', se=FALSE) 
+ xlim(-2,2) + ylim(-1,1) + 
  ggtitle('Regression Fit to Distribution A')

# points closer to line at middle
data = rB(10)
data %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method='lm', se=FALSE) + 
  xlim(-2,2) + ylim(-1,1) + 
  ggtitle('Regression Fit to Distribution B') 


# Points close to line at extremes
data = rC(10)
data %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method='lm', se=FALSE) + 
  xlim(-2,2) + ylim(-1,1) + 
  ggtitle('Regression Fit to Distribution C')


# residuals about line at extremes and below line at middle
data = rD(25)
data %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method='lm', se=FALSE) + 
  xlim(-2,2) + ylim(-1,1) + 
  ggtitle('Regression Fit to Distribution D')

#points tend to be closer to line at the neg end and further away at the 
# positive end

data = rE(10)
data %>% ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method='lm', se=FALSE) + 
  xlim(-2,2) + ylim(-1,1) + 
  ggtitle('Regression Fit to Distribution E')

slopes_A = replicate(1000, lm(rA(10))$coef[2] )
slopes_B = replicate(1000, lm(rB(10))$coef[2] )
slopes_C = replicate(1000, lm(rC(10))$coef[2] )
slopes_D = replicate(1000, lm(rD(10))$coef[2] )
slopes_E = replicate(1000, lm(rE(10))$coef[2] )
# if I were to ask how do youe expect the sd of the 
# slope coef to behavior based upon these 
# different error structure

#Particularly ordering amonst models
# What allows the line to vary the most


################################################################




#Ans less restriction at extremes
# remember has to pass through the xbar and ybar

c(A = sd(slopes_A), B = sd(slopes_B), C = sd(slopes_C), D = sd(slopes_D), E = sd(slopes_E))


# R Exercise

## Real Estate in Boston  

setwd("C:\\Berkeley\\DataSci203_Spring_2025\\temp203\\instructor_central\\live_session\\unit_09")


load('hprice1.RData') # provides 3 objects
head(data)

# Previously, last week, we fit a regression of price on square feet.

(model_1 = lm(price ~ sqrft, data = data))
plot(data$sqrft,data$price)
abline(model_1, col="red")
cor(data$price,data$sqrft)^2

# 1.Estimate a new model (and save it into another object) that includes the 
# size of the lot and whether the house is a colonial. This will estimate the model:

# New model price = Beta0 + Beta1*sqrtfoot Beta2*lotsize + 
  # Beta3*Colonial + epsilon

#model_2 <- lm(price ~ sqrft + lotsize + bdrms, data=data)

model_3 <- lm(price ~ sqrft + lotsize + colonial, data=data)

# Make a prediction: What do you think is going to happen to the coefficient that 
# relates square footage and price?
# <> Will the coefficient increase, decrease, or stay the same?
# <> Will the uncertainty about the coefficient increase, decrease, or stay the same?

# Bhat = [(X'X)^-1]X'Y
# Computing Var of Bhat we have 
# Var(Bhat) = [(X'X)^-1]X' M X [(X'X)^-1]
# constant residual error M = I*sigma^2
# if not a number of way of computing M

vcovHC(model_1)
vcovHC(model_1)^0.5
coeftest(model_1, vcov = vcovHC(model_1))
coeftest(model_1 )
summary(model_1)


# vcovHC(model_2)
# vcovHC(model_2)^0.5
# #coeftest(model_2, vcov = vcovHC(model_2))
# coeftest(model_2)
# summary(model_2)
# hist(model_2$residuals)
# skewness(model_2$residuals)
# kurtosis(model_2$residuals)
# #Normality Test
# shapiro.test(model_2$residuals)
# #Serial correlation
# Box.test(model_2$residuals,lag=5,type="Ljung-Box")
# # Test Breusch-Pagan test heteroscadasticity
# bptest(data$price ~ data$sqrft + data$lotsize + data$bdrms)
# # Test of normal based upon skewness and kurtosis
# jarque.test(model_2$residual)
# # Durbin Watson Serial Correlation
# dwtest(data$price ~ data$sqrft + data$lotsize + data$bdrms)

# residuals are what we look at
vcovHC(model_3)
vcovHC(model_3)^0.5
coeftest(model_3, vcov = vcovHC(model_3))
#coeftest(model_3)
#summary(model_3)
VIF(model_3)
skewness(model_3$residuals)
kurtosis(model_3$residuals)
hist(model_3$residuals)

# normal
dn<-rnorm(150)
kurtosis(dn)-3
# t Lepto
dt<-rt(150,3)
kurtosis(dt)-3

# normal
dnb<-rnorm(100,0.5,0.5)
kurtosis(dnb)-3
#uniform Platy
du<-runif(100)
kurtosis(du)-3

dat <- data.frame(x=dn, y=dt) 
plot(c(-2.5,8),c(0,0.5),type = "n",main="Leptokurtic Comparison")
lines(density(dat$x),col="red")
lines(density(dat$y),col="blue")

legend("topright", legend=c("Norm", "t"), col=c("red", "blue"), lwd=2)

dat2 <- data.frame(x=dnb, y=du)
m_dnor<-max(dnorm(dnb))
m_du<-max(dnorm(du))
plot(c(-2.5,8),c(0,1.5),type = "n", main="Platykurtic Comparison")
lines(density(dat2$x),col="red")
lines(density(dat2$y),col="blue")

legend("topright", legend=c("Norm", "Unif"), col=c("red", "blue"), lwd=2)



# #Normality Test based on order staistics or shape
shapiro.test(model_3$residuals)

# The Shapiro-Wilk test tends to be very sensitive to sample size.
# For small sample sizes (n < 30), it may not detect deviations from normality, 
#     even if they are substantial.
# For large sample sizes, even slight deviations from normality can result in a significant p-value, 
#      making the data seem non-normal, even when the deviation is practically negligible.

# # Test of normal based upon skewness and kurtosis
jarque.test(model_3$residual)

# #Serial correlation
Box.test(model_3$residuals,lag=5,type="Ljung-Box")
# # Durbin Watson Serial Correlation
dwtest(data$price ~ data$sqrft + data$lotsize + data$colonial)


# # Test Breusch-Pagan test heteroscadasticity
bptest(data$price ~ data$sqrft + data$lotsize + data$colonial)






#There are a variety of ways to evaluated a multiple regression

# Wald Test: The Wald test is a statistical test used to assess the significance of individual 
# coefficients or parameters in regression models, 
# particularly in the context of generalized linear models (GLMs) and logistic regression. 
# It evaluates whether a particular parameter is significantly 
# different from a specified value (usually zero). 
# It's often used to assess the significance of predictors in regression models.
# The Wald test assesses the significance of individual or groups of coefficients in a single model. 


# Likelihood Ratio Test: The likelihood ratio test is a statistical test used for comparing nested models. 
# It assesses whether a more complex model 
#provides a significantly better fit to the data than a simpler model. 
# It is commonly used in the context of maximum likelihood estimation and is 
#used to determine if adding or removing parameters from a model significantly improves or worsens the model fit.

# anova (lowercase): In R, anova is a generic function used for performing analysis of variance (ANOVA) tests. 
# ANOVA is typically used to compare means among multiple groups to determine if there are statistically significant differences. 
# You can use anova in various contexts, such as one-way ANOVA, 
# two-way ANOVA, or mixed-design ANOVA, depending on your study design.



# Anova (capitalized): The Anova function, often found in the car package in R, is used for conducting 
# type-II and type-III ANOVA tests. 

# The two models can be thought as two groups



a<-anova(model_1, model_3, test='F')
a

# Perform added sum of squares test
#Added Sum of Squares Test (Partial F-Test):

#This test compares two nested models to determine 
#if the additional predictors in the more complex model significantly improve the fit:​

#Model 1: price ~ sqrft​

#Model 3: price ~ sqrft + lotsize + colonial​

#The test evaluates whether the predictors lotsize and colonial contribute significantly beyond sqrft by 
#comparing the reduction in the residual sum of squares (RSS) between the models. The F-statistic is calculated as


#F=((RSS1−RSS2)/(p2−p1))/(RSS2/(n−p2))

#where RSS1 and RSS3 are the residual sums of squares for Model 1 and Model 3, respectively; 
#p1​ and p2 are the number of parameters in each model; and n is the sample size.


#Reduced Model Residual SSQ
rss_1<-sum(model_1$residuals^2)
rss_1

#Full model Residual SSQ
rss_3<-sum(model_3$residuals^2)
rss_3

# Ho Beta2 = Beta3 = 0
fStar<-((rss_1-rss_3)/(model_1$df.residual-model_3$df.residual))/(rss_3/model_3$df.residual)
fStar
1-pf(fStar,model_1$df.residual-model_3$df.residual,model_3$df.residual)

#> The HC0 (Heteroskedasticity-Consistent Covariance Matrix) standard errors, along with other heteroskedasticity-robust 
#> standard errors, were developed by Halbert White, an American econometrician. White introduced the concept of 
#> heteroskedasticity-robust standard errors in his seminal paper titled "A Heteroskedasticity-Consistent Covariance 
#> Matrix Estimator and a Direct Test for Heteroskedasticity" published in 1980.

#Type II ANOVA:
  
#> In Type II ANOVA, each factor's contribution to the model is assessed while ignoring the presence of other factors. 
#> This means that the sums of squares attributed to each factor are adjusted for all other factors in the model 
#> except the one being tested.
#> 
#> Type II ANOVA is robust to unbalanced designs, meaning it gives valid results even if the sample sizes across 
#> levels of factors are unequal.

#> However, Type II ANOVA does not account for interactions between factors. Therefore, the sums of squares for 
#> main effects may depend on the presence or absence of other factors in the model.
#>
#>The Wald test assesses the significance of individual coefficients (or groups of coefficients) by 
#>comparing them to zero.
#>
#>The F test compares the fit of two nested models (full vs. reduced) to determine if the 
#>additional parameters improve the model fit significantly.
#>
#> 
#> F statistic
A<-Anova( model_3,type="II", vcov = vcovHC(model_3, type = "HC0",test.statistic="F"))
A


A<-Anova( model_3,type="II", vcov = vcovHC(model_3, type = "HC0",test.statistic="Wald"))
A

# Wald Test
#In this context, it tests the null hypothesis that the coefficients of lotsize and colonial are zero in the full model
#H0:βlotsize=βcolonial=0
.
waldtest(model_1, model_3, vcov = vcovHC(model_3, type = "HC0"))

#waldtest(model_1, model_3)

# note the Wald and the partial F test (anova) give identical results here for the
# non robust testing
#F and chisq are closely related

#Chi-Squared Distribution: If U is a random variable following a chi-squared distribution with 
# d1 degrees of freedom U∼χ^2 d1, it represents the sum of the squares of d1 independent standard normal variables

#F-Distribution: If U1 and U2 are independent random variables such that U1∼χ^2(d1) and U2∼χ^2(d2), 
#then the random variable defined by the ratio

#F=(U1/d1)(U2/d2)

#follows an F-distribution with d1 and d2 degrees of freedom (F∼F(d1,d2)).

# 2.Use the function vcovHC from the sandwich package to estimate 
# (a) the the heteroskedastic consistent (i.e. robust) variance covariance matrix; 
# and (b) the robust standard errors for the intercept and slope of this regression. 
# Recall, what is the relationship between the VCOV and SE in a regression?

#vcovHC(model_2)


#get_robust_se <- function(model) {
#   se <- sqrt(diag(vcovHC(model)))
#   return(se)
# }

get_robust_se_variable <- function(model, variable) { 
  se <- sqrt(diag(vcovHC(model)))[variable]
  return(se)
}

rse_sqrft <- get_robust_se_variable(model_3, variable = 'sqrft')
rse_sqrft

# 3. Perform a hypothesis test to check whether the population relationship between 
#    sqrft and price is zero. Use coeftest() with the robust standard errors computed 
#    above.

coeftest(model_3, vcov=vcovHC(model_3))

# 4. Use the robust standard error and qt to compute a 95% confidence interval 
#    for the coefficient sqrft in the second model that you estimated. 
#    price=β0+β1sqrft+β2lotsize+β3colonial.
#df = nobs - (predictors +1) #(include the intercept)
t_val <- qt(.975, df=nrow(data)-length(model_3$coefficients))
t_val

qnorm(0.975)

coef_sqrft <- model_3$coefficients['sqrft']
coef_sqrft


low_ci     <- coef_sqrft - (t_val * rse_sqrft)
high_ci    <- coef_sqrft + (t_val * rse_sqrft)

c(low_ci, high_ci)

# function to perform this
get_robust_ci <- function(model, variable, p_value) { 
#   ## This function computes the degrees of freedom from the model 
#   ## Args: 
#   ##  - variable the variable that we're interested in 
#   ##  - p_value the p_value we'd like to compute 
#   ## 
#   ## It also uses the get_robust_se_varaible that we wrote above. 
#   ##  - We *could* do it again by hand, but why if we've already writen it? 
#   
#   ## Preliminary pieces 
    df <- model$df.residual
    alpha_by_two <- p_value / 2
#   
#   ## Pull robust standard errors using helper we wrote earlier.   
    rse <- get_robust_se_variable(model, variable)
#   
#   ## Finally, compute the CI. Note that in the qt call, we're passing the 
#   ## \frac{\alpha}{2} and (1 - \frac{\alpha}{2}).
    ci <- model$coefficients[variable] + qt(c(alpha_by_two, 1-alpha_by_two), df = df) * rse
#   
    return(ci)
  }

get_robust_ci(model_3, 'sqrft', 0.05)

# 5. Bootstrap 
# A self starting process continues with external input
# Use 'bootstraps' to remove yourself from for example the mud
# The (Surprising) Adventures of Baron Munchausen 1988 Movie

# Published by Brad Efron "An introduction to Boot Strapping"
# "Bootstrapping is any test or metric that uses random sampling with replacement 
# (e.g. mimicking the sampling proces), and falls under the broader class of 
# resampling methods. Bootstrapping assigns measures of accuracy (bias, variance, 
# confidence intervals, prediction error, etc.) to sample estimates." Wikipedia

# Bootstrap methods are alternative approaches to traditional hypothesis testing and are notable 
# for being easier to understand and valid for more conditions.
# Both bootstrapping and traditional methods use samples to draw inferences about populations. 
# To accomplish this goal, these procedures treat the single sample that a study obtains as only 
# one of many random samples that the study could have collected.
# Introduction to Bootstrapping in Statistics with an Example
# By Jim Frost 

bootstrap_sqft <- function(d = data, number_of_bootstraps = 1000) {
  number_of_rows <- nrow(d)

  coef_sqft <- rep(NA, number_of_bootstraps)

  for(i in 1:number_of_bootstraps) {
    bootstrap_data <- d[sample(x = 1:number_of_rows, size = number_of_rows, replace = TRUE), ]
    estimated_model <- lm(price ~ sqrft + lotsize + colonial, data = bootstrap_data)
    coef_sqft[i]    <- coef(estimated_model)['sqrft']
  }
  return(coef_sqft)
}

res <- bootstrap_sqft(number_of_bootstraps = 1000)

plot(density(res))

mean(res)

#Compute the standard deviation of the bootstrapped regression coefficients.
# How does this compare to the robust standard errors you computed above?

sd(res)

#compare to
get_robust_se_variable(model_3, 'sqrft')
