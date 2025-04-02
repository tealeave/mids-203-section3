library(tidyverse)
library(ggplot2)
#library(magrittr)


## Understanding Sampling Distributions 

# - Let $X$ be a Bernoulli random variable representing an unfair coin with $P(X=1) = 0.7$.
# - You have an iid sample of size 2,  $(X_1,X_2)$.
# - Compute the sampling distribution of $\overline X = \frac{X_1+X_2}{2}$.


# ggplot() + 
#   aes(x = c(0,5), y = c(0,1)) + 
#   geom_blank() + 
#   theme_bw() + 
#   theme(
#     panel.background = element_rect(fill = "transparent",colour = NA),
#     plot.background = element_rect(fill = "transparent",colour = NA)
#   )

######################################################################

## Sampling from the Bernoulli Distribution in R 
# - To demonstrate the CLT, we chose a Bernoulli distribution with parameter $p$.
# - This distribution is very simple
# - This distribution is non-normal, and can be very skewed depending on $p$.

# - First, set `p=0.5` so your population distribution is symmetric. 
#   Use a variable `n` to represent your sample size.  Initially, set `n=3`



# Create a sample of 50 numbers which are incremented by 1.


 # Bernoulli/Binomial
x <- seq(0,50,by = 1)
print(x)
# x is the number of success
# from flipping a fair toss of a fair coin 50 times
# Create the binomial distribution.



# the mass or probability at each value of the supp
# arguments: quantile (or vector of realizations) , number of trials or number of coins in a trial
y <-  dbinom(x,50,0.5)
print(y)

# the CDF
y2 <- pbinom(x, 50, 0.5)
print(y2)

par(mfrow=c(1,2))
# Plot the graph for this sample.
plot(x,y)
plot(x,y2)

par(mfrow=c(1,1))

# more simply
#quantile (or vector of realizations) , number of trials or number of coins in a trial
dbinom(0,1,prob=0.5) # bernoulli
dbinom(0,2,prob=0.5)

#Quantile, number of trials CDF
dbinom(0,3,prob=0.5)
dbinom(1,3,prob=0.5)
dbinom(2,3,prob=0.5)


pbinom(2,3,p=0.5)



# Deviates or random number
## Useful R Commands, Part I

# - R doesn't have a `bernoulli` function.
# - To simulate draws from a Bernoulli variable, you can either: 
#  - Use `sample` 
#  - Or, use `rbinom` (the Bernoulli distribution is a special case of a binomial distribution.  In this function, `size` refers to a distribution parameter, not the number of draws.)

#lets set
n <- 3
p <- 0.5
size = 1

n
p

sample(x=0:1, size=n, replace=TRUE, prob=c(1-p, p))

# Finding random values from a binomial
# Each are a Bernoulli
# , i.e. n Bernoulli


#n: The number of random values (or experiments) you want to generate.
#size: The number of trials in each experiment.
#prob: The probability of success on each trial.

# random deviates
rbinom(n=n, size=1, prob=p)

# n	number of observations or times the experiment is run
# size	number of trials (one or more) per observation
# Bernoulli would be size 1

## Useful R Commands, Part II 

# - To repeat an action, you can use `replicate`
replicate(10, rbinom(n=n, size=1, prob=p))

## Useful R Commands, Part III 

# - To quickly visualize your results, try `hist`

hist(x = rnorm(100), main = "Simulated Sample Means")


## Useful R Commands, Part IV

# - Or, to work with `ggplot` store these results in a data.frame.  

d <- data.frame(x = rnorm(100))
d %>% 
  #The aes() function in ggplot2 (R) is used to map variables to aesthetic properties of a plot,
  ggplot(aes(x=x)) + 
  geom_histogram()


## Exercises part 1



# Throughout this part, we will use fair coins (`p = 0.5`).
#  n becomes size or the number of coins tossed in a trial

# 1. Write code (using either `sample` or `rbinom` that simulates 
#    taking three coins (`n=3`) and tossing them once. For this "collective toss", 
#    use the `mean` function to compute the sample mean -- the average of the number of heads 
#    that are showing.  
#    Make sure that when you run it, you get sample means in {0,1/3,2/3,1}
               
# note the toss_coin produce the mean of sample of bernoulli
# the binomial rv is the sum of the n bernoullis



# so if a bernoulli pmf can be written as p^k(1-p)^(1-k) where k ={0,1}, 
# each iteration is
# one trial n= 1

# if an iteration consists of n>1 trials and p remains constant and each of trial
# outcomes is independent from each of the other trial outcomes we can think of the 
# the set of trials as n bernoullis which is the same as a binomial(n,p) and 
# k is the number of success ==> k = {0,1,2,...,n}

# binomial pmf is n!/((n-k)!*k!) * p^k*(1-p)^(n-k)

# we are creating our binomial samples in the function below as a set of bernoulli
# trials


toss_coins <- function(number_of_tosses = 10, number_of_coins = 3, prob_heads = 0.5) { 
           ## function arguments
           ##   - number_of_coins: the number of coins that you're tossing
           ##   - prob_heads: the probability that each coin comes up heads
                 
               replicate(n = number_of_tosses,sample(
                     x = 0:1, 
                     size = number_of_coins, 
                     replace = TRUE,
                     prob = c(1-prob_heads, prob_heads)
                   ) %>% mean
                 )
                 
               }
               
               
            
toss_coins()
cT<-toss_coins(
  number_of_tosses = 10, 
  number_of_coins  = 3,
  prob_heads       = 0.5
)
cT
mean(cT)

# Once again 
# note n is the number of coins in a toss
# The random variable is a value in the output sample {0,1,...,n}
# The mean of a set of Bin random variable is np is the expected number of successes
# where p is the  prob of success n is the number of coins in a trial
# The mean of the of the mean of each number of success  or sum( of n X's) = p

# 2. The sample mean is a random variable. To understand it, use the 
#    visualization trick from a few weeks ago.  Use the `replicate` function to run the above experiment 1000 times, and plot a histogram of the results.

# tossing 3 coins 1000 times
# What values would you get?
toss_coins(10000, 3, .5) %>% hist(main = 'Histogram of 3 coins')

# 3. If you increase the number of replications enough, will the distribution ever look normal?  Why or why not?
  
#  > No it never seems to look normal. This is because we're only tossing three coins so there are very few possible outcomes

# mean of a Binomial == n*p (where in this case is the coins in a trial)
# note that the binomial is the sum of the RV of n bernoulli so the bin is in the space {0,1,...n}
# 
# The toss_coin produces the mean of the n beroulli's whose E[Xbar] 
#  = n*p/n == p

# The variance of sum of n bernoullis == binomial = n*p*q where p+q =1 or q = 1-p

# The variance of the mean = Var(Xbar) = Var(X/n) = n*p*q/n^2 = p*q/n
# 
vals<-toss_coins(1000, 3, .5)
# 
mean(vals)
var(vals)
n*p*(1-p)/n^2
sd(vals)


vals<-toss_coins(1000, 300, .5)
hist(vals)
mean(vals)
sd(vals)^2



# 4. Use `sd()` to check the standard deviation of the sampling distribution 
#    of the mean for `n=3`.  What sample size is needed to decrease the standard 
#    deviation by a factor of 10?  Check that your answer is correct.

#> I'm keeping the number of tosses constant at 1000, so that we have a reasonable "filled 
#   in" histogram and can see the shape of the sampling distribution. 
#   When I conduct 5 tosses, there isn't really enough information for me to be sure 
#   that this is actually normal. It still looks a little bit different. Let's try 
#   running this at `n=15`. 
par(mfrow=c(2,2))
toss_coins(number_of_tosses = 1000, number_of_coins = 15, prob_heads = .5) %>% 
  hist()

# > Now, depending on the toss that I conduct, it is starting to look normal. 
# I wonder, though, what would happen if we continued to increase this? 
  

toss_coins(number_of_tosses = 1000, number_of_coins = 100, prob_heads = .5) %>% 
  hist(main = '1000 Tosses of 100 Fair Coins')


# > Now we're starting to look good! I'll just increase to 1000 to be sure. 


toss_coins(number_of_tosses = 1000, number_of_coins = 10000, prob_heads = .5) %>% 
  hist(main = '1000 Tosses of 1,000 Fair Coins')

par(mfrow=c(1,1))

## Exercises Part 3

# For this part, we'll study a very unfair coin. `p = 0.01`.  

# This is an example of a highly skewed random variable.  That roughly means that 
# one tail is a lot longer than the other.

# For this activity, you can simply use your eyes to gauge how skewed a distribution is.  
# If you prefer, you can also use the skewness command in the univar package to measure 
# skewness.  You may hear a rule of thumb that a skewness above 1 or below -1 is a highly 
# skewed distribution.

# 6. Start with n=3 as before.  What do you notice about the shape of the sampling 
# distribution?


toss_coins(number_of_tosses = 1000, number_of_coins = 3, prob_heads = 0.01) %>% 
  hist(main = '1000 Tosses of 3 Skewed Coins')

# > very skewed!

# 2. Try increasing the sample size -- i.e. the number of tosses. 
#    Is there a point that this starts to match the famous rule of thumb? 

par(mfrow=c(2,2))
toss_coins(number_of_tosses = 1000, number_of_coins = 30, prob_heads = 0.01) %>% 
  hist(main = '1000 Tosses of 100 Skewed Coins')

toss_coins(number_of_tosses = 1000, number_of_coins = 100, prob_heads = 0.01) %>% 
  hist(main = '1000 Tosses of 100 Skewed Coins')

toss_coins(number_of_tosses = 1000, number_of_coins = 1000, prob_heads = 0.01) %>% 
  hist(main = '1000 Tosses of 1000 Skewed Coins')

toss_coins(number_of_tosses = 1000, number_of_coins = 10000, prob_heads = 0.01) %>% 
  hist(main = '1000 Tosses of 10,000 Skewed Coins')

# > It definitely does approach a normal distribution (yay CLT works!) 
# but it required a much large n to get there.

## Discussion Questions

# 1. How does the skewness of the population distribution affect the applicability of 
#    the Central Limit Theorem?  What lesson can you take for your practice of statistics?

#> It seems like the skewness makes it harder for the CLT to function. 
#  When we increased the skewness of the coin that we were tossing, it required more and more data for the sampling distribution to start looking normal. And, in fact, when we made the probability of heads be only 1%, it didn't really ever start to look normal. 

# 2. Name a variable you would be interested in measuring that has a substantially 
#    skewed distribution.

# > Political donations in the 2020 election? 
  
# 3. One definition of a heavy tailed distribution is one with infinite variance.  
#    For example, you can use the `dcauchy` command in R to take draws from a 
#    Cauchy distribution, which has heavy tails.  Would the previous exercise work 
#    if you did this?  How do you know?
  
#  > It sure seems to me that if we have substantial skew in the data, 
#    that the CLT won't work. But, I'd like to point out that the skew *does* 
#    need to be substantial. Even if there is just a 5% chance of the coin coming up 
#    heads (rather than the 1% that we've been working with) the distribution looks 
#    much more normal. 