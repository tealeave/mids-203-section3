library(tidyverse)
library(wooldridge)
library(car)
library(lmtest)
library(sandwich)
library(stargazer)

# Note if log(1 + x )around x(0) = 0 is small
# by Taylor Expansion Theorem 
# log(1 + r) = r + O(x^2)

log(1.01)  # log(1 + 0.01)
log(1.001) # log(1 + 0.001)
log(1.02)  # log(1 + 0.02)
log(1.1)


?wage1
names(wage1)

library(gridExtra)

# Histogram of Educ
p1 <- ggplot(data = wage1, aes(x = educ)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(breaks = seq(0,20,2))+
  labs(title = "Hist Years of Ed",x = "Years of eductaion",y = "Count")

# Histogram of Wage
p2 <- ggplot(data = wage1, aes(x = wage)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(breaks = seq(0,30,2))+
  labs(title = "Hist  Hourly Earning", x = "Hourly wage", y= "Count")

#Scatter plot of X = educ, Y= Wage 
p3 <- ggplot(data = wage1, aes(x = educ, y = wage)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Hourly wage incr w/ ed", x = "Years of education", y = "Hourly wage")

#Scatter plot of X = educ, Y= log Wage 
p4 <- ggplot(data = wage1, aes(x = educ, y = lwage)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Log of Hourly wage incr w/ ed", x = "Years of education", y = "Log Hourly wage")

grid.arrange(p1, p2,p3,p4, nrow = 2, ncol = 2)

# adding two other features
# Hist years of experience
p5<-ggplot(data=wage1, aes(x=exper)) +
  geom_histogram(bins=10) + 
  scale_x_continuous(breaks = seq(0,20,5)) + 
  labs(title = "Most people have at least 17 years of experience", x = "Years of experience",
       y = "Count")

# Hist of tenure
p6<-ggplot(data=wage1, aes(x=wage)) +
  geom_histogram(bins=10)  + 
  scale_x_continuous(breaks = seq(0,30,2)) + 
  labs(title = "Most people earn at most $14 per hour", x = "Hourly wage",
       y = "Count")

# Smoothing done by loess
# Plot wage versus experience
p7<-ggplot(data=wage1, aes(x=exper,y=wage)) +
  geom_point() + 
  geom_smooth(se=FALSE) + 
  labs(title = "A nonlinear association between wage and experience", x = "Years of experience",
       y = "Hourly Wage")

# Plot log wage vs experience (note more controlled variance spread)
p8<-ggplot(data=wage1, aes(x=exper,y=lwage)) +
  geom_point() + 
  geom_smooth(se=FALSE) + 
  labs(title = "A nonlinear association between log of wage and experience", x = "Years of experience",
       y = "Log Hourly Wage")

grid.arrange(p5, p6,p7,p8, nrow = 2, ncol = 2)

# Hist wage and log wage
p9 <- ggplot(data = wage1, aes(x = lwage)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(breaks = seq(0,30,2))+
  labs(title = "Log Wage Histogram", x = "Log Hourly wage", y = "Count")
p10 <- ggplot(data = wage1, aes(x = wage)) +
  geom_histogram(bins = 10) +
  scale_x_continuous(breaks = seq(0,30,2))+
  labs(title = "Raw Wage Histogram ", x = "Hourly wage", y = "Count")

grid.arrange( p10,p9, nrow = 2, ncol = 1)



# Plots of wage against experience
p12<-ggplot(data = wage1, aes(x = exper, y = wage)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Non-linear betw exper vs hrly wage", 
       x = "Years of experience", y = "Average of hourly wage")

# plot of wage against tenure
p13<-ggplot(data = wage1, aes(x = tenure, y = wage)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Non-linear betw tenure and hrly wage", 
       x = "Years with current employer ", y = "Average of hourly wage")

grid.arrange( p12,p13, nrow = 2, ncol = 1)
# NOTE curvi-linearity

#interpretaion of log y and linear and polynomial X
model1 <- lm(lwage ~ educ + exper + tenure , data = wage1)
coeftest(model1, vcov = vcovHC)
summary(model1)

# Treating exprience as categorical variable
model_0 <- lm(lwage ~ factor(exper),data=wage1)
coeftest(model_0)
ggplot(data = wage1, aes(x=exper, y=model_0$fitted.values )) +
  geom_point() +
  labs(title = "Fitted Values VS Exper as Factor", x = "Exper (years as factor)", y = "Fitted Values")

# Thought how to handle increase and curvi-linear
# going up at descreasing rate

# linear and order 2
model2 <- lm(lwage ~ educ + exper + I(exper^2) + tenure + I(tenure^2) , data = wage1)
# I() "as is" rather than being treated as a part of the model formula syntax.
# for example exper^2 or tenure^2 above without I() 
# would be interpreted as an interaction term between exper (or tenure)  and itself, which is just 
# exper (or tenure).


coeftest(model2, vcov = vcovHC)
summary(model2)  # note degrees of freedom

model2a <- lm(lwage ~ educ + exper + exper^2 + tenure + tenure^2 , data = wage1)
# I() "as is"
coeftest(model2a, vcov = vcovHC)
summary(model2a)  # note degrees of freedom

# captures the curvi-linearity
par(mfrow=c(2,1))
plot(wage1$exper, model1$fitted.values)
plot(wage1$exper, model2$fitted.values)
par(mfrow=c(1,1))


par(mfrow=c(2,1))
plot(model1$fitted.values, model1$residuals)
plot(model2$fitted.values, model2$residuals)
par(mfrow=c(1,1))

# Interpreting coefficients, say experience

# Recall an Additive or multiplicative effect?

# β2×experience+β3×experience^2
# β2 is the linear effect of experience on the outcome.
# 
# When experience is small, the effect on the outcome is primarily driven by the linear term, 
# β2×experience
# 
# β2 can be interpreted as the initial rate of change in the outcome with respect to experience.
# 
#
# β3 is the quadratic effect of experience on the outcome, meaning it captures how the 
#  effect of experience changes with increasing experience.
# 
# A positive β3 would mean that as experience increases, the effect of experience on the 
# outcome accelerates (increasing more quickly).

# A negative β3 would imply a diminishing effect of experience, indicating that as experience grows, 
# the additional effect on the outcome slows down.
# So, β3 adjusts how this effect changes as experience accumulates, indicating 
# either an accelerating or decelerating relationship.

# On top of this need to figure out how one unit change exffects y?


#indicator
dwage1 <- wage1 %>%
  mutate( diploma = as.numeric(educ >=12))

model6 <- lm(lwage~educ + diploma, data=dwage1)
coeftest(model6, vcov = vcovHC)

#categorical
model7<-lm(lwage ~ female + educ,data=dwage1)
coeftest(model7, vcov = vcovHC)

model2 <- lm(lwage ~  educ + I(female*educ), data = wage1)
coeftest(model2, vcov = vcovHC)
 
model3 <- lm(lwage ~ female + educ + exper  + tenure + I(exper^2)  + I(tenure^2)
             , data = wage1)
coeftest(model3, vcov = vcovHC)

model4 <- lm(lwage ~ female + educ + I(female*educ) + exper  + tenure + I(exper^2) 
             + I(tenure^2) , data = wage1)
coeftest(model4, vcov = vcovHC)

dwage1 <- wage1 %>%
  mutate( diploma = as.numeric(educ >=12))

model5 <- lm(lwage ~ female + educ + exper  + tenure + I(exper^2) 
             + I(tenure^2)  + diploma  , data = dwage1)

coeftest(model5, vcov = vcovHC)
summary(model5)

wage1 %>%
  mutate(diploma = educ >=12) %>%
  group_by(diploma) %>%
  summarise(count = n())


se.model1 = coeftest(model1, vcov = vcovHC)[ , "Std. Error"]
se.model2 = coeftest(model2, vcov = vcovHC)[ , "Std. Error"]
se.model3 = coeftest(model3, vcov = vcovHC)[ , "Std. Error"]
se.model4 = coeftest(model4, vcov = vcovHC)[ , "Std. Error"]
se.model5 = coeftest(model5, vcov = vcovHC)[ , "Std. Error"]

stargazer(model1, model2, model3, model4, model5, type = "text", omit.stat = "f",
          se = list(se.model5),
          star.cutoffs = c(0.05, 0.01, 0.001), title = "Table 1: The relationship between hourly wage and years of education")
