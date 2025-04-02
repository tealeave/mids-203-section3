# "Unit 8 Live Session: Regression Theory"

library(tidyverse)
library(sandwich)
library(lmtest)
library(ggplot2)
library(zoo)
library(broom)
library(psych)
#library(testthat)

# Frisch-Waugh (1933) - Lovell (1963) Theorem
# FWL is a  fundamental result in linear regression analysis, 
# demonstrating that the coefficients of a subset of explanatory variables in a multiple regression 
# can be obtained through a series of simpler regression steps, namely regressing on a subset of the 
# set of features.  Then taking the residuals and regressing them on the remainin feature.  
# It is a means of partitioning the problem


# Claim: that we can re-represent a coefficient that
# we're interested in as a function of all the other variable in a regression.

# Y = Bhat_0 + Bhat_1*X(1) + Bhat_2*X(2) + Bhat_3*X(3) + e

# and let 

# X(1) = Lhat_0 + Lhat_2*X(2) + Lhat_3*X(3) +r(1)

# Y = Ghat_0 + Ghat_1*r(1)

# By virtual of no perfect multicolinearity we can work the following result

# Bhat_1 = Ghat_1

# Demonstration
#Suppose that the population model is the following:
#  Y = -3 + (1 ? X1) + (2 ? X2) + (3 ? X3)

# d <- data.frame(
#   x1 = runif(n=100, min=0, max=10),
#   x2 = runif(n=100, min=0, max=10),
#   x3 = runif(n=100, min=0, max=10)
# )
# ## because we know the population model, we can produce a single sample from it
# ## using the following code:
# d <- d %>%
#   mutate(y = -3 + 1*x1 + 2*x2 + 3*x3 + rnorm(n=n(), mean=0, sd=1))
# head(d)
# 
# #random noise is included, Why?
# 
# #  By assumption, what we have measured in this world, X1, X2, X3 
# # are uncorrelated with these other features. error is exogeneous
# 
# 
# dim(d)
# 
# model_main <- lm(y ~ x1 + x2 + x3, data = d)
# coef(model_main)
# 
# # The claim is that we can produce an estimate of ??1 using an auxiliary set of
# # regression estimates, and then using the regression from that auxiliary regression.
# 
# model_aux <- lm(x1 ~ x2 + x3, data = d)
# coef(model_aux)
# 
# #Finding residuals. Definition?
# 
# d %>%
#   ggplot() +
#   aes(x=x1, y=y) +
#   geom_point() +
#   geom_segment(aes(x=0, xend=10, y=0, yend=50), color = 'steelblue')
# 
# 
# # In order to access these residuals, we can "augment" the dataframe that we used
# # in the model, using the broom::augment function call.
# 
# d_augmented <- augment(model_aux)
# d_augmented$y <- d$y
# d_augmented
# 
# model_two <- lm(y ~ .resid, data = d_augmented)
# coef(model_two)
# 
# # Our claim was that the coefficients from model_main and model_two should be
# # the same.
# 
# 
# as.numeric(coef(model_main)['x1'])
# as.numeric(coef(model_two)['.resid'])
# -------------------------------------------------------------------------------------
d<-mtcars
colnames(d)
#Suppose `x` and `y` are variables in dataframe `d`.

y<-d$mpg
x<-d$wt

d<-cbind(d,y,x)
#To fit an ols regression of Y on X:
  
  mod <- lm(y ~ x, data = d)

# To access **coefficients** from the model object:
  
  mod$coefficients
  coef(mod)

# To access **fitted values** from the model object:
  
  mod$fitted
  fitted(mod)
  predict(mod)

# To access **residuals** from the model object:
  
  mod$residuals
  resid(mod)

# To create a scatterplot that includes the regression line:
  
  plot(d[,'x'], d[,'y'])
  abline(mod)
#or 
  
  d %>% 
    ggplot() + 
    aes(x = x, y = y) + 
    geom_point() + 
    geom_smooth(method = lm)
  
-----------------------------------------------------------------------------  
# R Exercise
  
## Real Estate in Boston  
  
setwd("C:\\Berkeley\\DataSci203_Spring_2025\\temp203\\instructor_central\\live_session\\unit_08")
  
  
load('hprice1.RData') # provides 3 objects

#descriptive analysis
head(data)

str(data)
describe(data)
summary(data)



# Suppose that you're interested in knowing the relationship between price and 
# square footage. 

# **1.** Create a scatterplot of `price` and `sqrft`. Like every plot you make, 
# ensure that the plot *minimally* has a title and meaningful axes. 


data %>% 
  ggplot(aes(x=sqrft, y=price))+
  geom_point() +
  ggtitle("Scatterplot") +
  labs(
    title = 'Square Footage vs. Price', 
    x ='House Size Square Footage',
    y = 'Price in 1,000s')

#
# **2.** Find the correlation between the two variables.


# Pearson correlation between 2 variables
r<-cor(data$sqrft, data$price)
print("r: ")
print(r)

r2<- cor(data$sqrft, data$price)^2
print("r^2: ")
print(r2)

data %>% 
  summarise(
    calculated_cor = cor(sqrft, price)
  )

# **3.** Recall the equation for the slope of the OLS regression line

cov(data$sqrft,data$price)/var(data$sqrft)

data %>% 
  summarize(
    'By Hand Rergression' = cov(sqrft, price) / var(sqrft)
  )
    

# 4:Run Regression price regressed upon sqft
# price = Beta0 + Beta1*sqrtfoot + epsilon


model_1 <- lm(price~sqrft, data=data)
model_1


#str(model_1)


# Bhat = [(X'X)^-1]X'Y
# Computing Var of Bhat we have 
# Var(Bhat) = [(X'X)^-1]X' M X [(X'X)^-1]
# constant residual error M = I*sigma^2
# if not a number of way of computing M

# Variance covariance matrix assuming homoscadesity
vcov(model_1)
vcov(model_1)^0.5


# Robust variance covariance matrix assuming heteroscadesity
vcovHC(model_1)
vcovHC(model_1)^0.5

# Hypothesis test of coefs under homo errors
coeftest(model_1)
vcov(model_1)^0.5

# Hypothesis test of coefs under hetero errors
coeftest(model_1, vcov = vcovHC(model_1))
vcovHC(model_1)^0.5


# **5.** Create a scatterplot that includes the fitted regression.
# Band is error around fitted value

data %>% 
  ggplot(aes(x=sqrft, y=price)) + 
  geom_point() + 
  labs(
    title = 'Scatter Plot of Square Footage and Price', 
    x = 'House Size Square Footage', 
    y = 'Price in 1,000s') + 
  geom_smooth(method = lm)


# If you wanted to include that scatter plot, but do it without relying 
# on the internals of `ggplot` you could use the `predict` function. 

data %>% 
  mutate(y_hat = predict(model_1)) %>% 
  ggplot(aes(x=sqrft, y=price)) + 
  geom_point() + 
  geom_point(aes(x=sqrft, y=y_hat), color = 'red')


# For each additional square foot, how much more (or less) is the house worth? 
# Each additional square foot is associated with $140 increase to the 
# price of the house. Why

# 7 New model price = Beta0 + Beta1*sqrtfoot Beta2*lotsize + 
# Beta3*Colonial + epsilon

# Prediction: The coefficient related to square footage and price is 
# likely to decrease since its effect would be absorbed somewhat by the 
# additional two features.  When we say, "absorbed" we mean, that since we 
# think there is some positive covariance between lotsize and squarefootage 
# of the house -- larger houses are on larger lots -- the effect of these 
# two variables might be split between the two. 

model_2 <- lm(price ~ sqrft + lotsize + colonial, data=data)
coef(model_2)
coeftest(model_2)

# What does the colonial mean

#vcov(model_2)^0.5

vcov(model_2,vcov = vcovHC(model_2)) 
# Hypothesis test of coefs under hetero errors
coeftest(model_2, vcov = vcovHC(model_2))

summary(model_2)$r.squared 
#R^2(adj) =1-[(1-R^2)*(n-1)/(n-k-1) ]
summary(model_2)$adj.r.squared


# What about the moment constraint
# sum of e's = 0
sum(model_2$residuals)

# covariance(predictors,residuals)

cov(data$sqrft,    model_2$residuals)
cov(data$lotsize,  model_2$residuals)
cov(data$colonial, model_2$residuals)



