library(tidyverse)
library(ggplot2)
# install.packages('patchwork')
library(patchwork)

rnorm(n = 1, mean = 2, sd = 1)
# individual




# larger draw 1
normal_draws <- rnorm(n = 1e2, mean = 2, sd = 1)
normal_draws
hist(normal_draws)



simulation<-replicate(10, expr = rnorm(n=1,mean=2,sd=1))
simulation


# What does it look like if you take 10 draws from this distribution and 
# plot it on a histogram?

# Density

d <- data.frame(x = rnorm(n = 10, mean = 2, sd = 1)) %>%
   mutate(
     y = dnorm(x = x, mean = 2, sd = 1)
   )

 plot_histogram <- d %>%
   ggplot() +
   aes(x = x) +
   geom_histogram() +
   coord_cartesian(xlim = c(-2, 6))
 plot_density <- d %>%
   ggplot() +
   aes(x = x, y = y) +
   geom_line(color = 'red') +
   coord_cartesian(xlim = c(-2, 6))
#
#
 plot_histogram | plot_density



# 
# 
#   - What happens if you take 10,000 draws from the distribution?  
#   - At what point does the histogram start to resemble the density plot?
#   
#   > In the following chunk (which you should recognize from above) 
#   we've changed the `n = 10` to `n=10000` in the creation of the data and then 
#   re-run the plotting calls. 
# > I think that it is fair to say that this is clearly normally distributed. 


d <- data.frame(x = rnorm(n = 10000, mean = 2, sd = 1)) %>% 
  mutate(
    y = dnorm(x = x, mean = 2, sd = 1)
  )

plot_histogram <- d %>%
  ggplot() +
  aes(x = x) +
  geom_histogram() +
  coord_cartesian(xlim = c(-2, 6))
plot_density <- d %>%
  ggplot() +
  aes(x = x, y = y) +
  geom_line(color = 'red') +
  coord_cartesian(xlim = c(-2, 6))

plot_histogram | plot_density

#dat_hist <- data.frame(short_simulation)
#dat_hist <- data.frame(d[,1])
Simulation<-d[,1]

true_density <- function(x) {
  dnorm(x = x, mean = 2, sd = 1)
}

dat_hist %>%
  ggplot() +
  geom_histogram(
    aes(x = Simulation, y = ..density..)) +
  stat_function(
    aes(x = Simulation), fun = true_density, color = 'darkred')



#At what point does it start to "look" normally distributed?
#If we've got 10,000 draws already in the data called `d` 
#what do you think about subsetting 
#that 10,000 observation simulation to see what plots look like at smaller draws? 


plot10<-d %>% 
  sample_n(size = 10, replace = TRUE) %>% 
  ggplot() + 
  aes(x = x) + 
  geom_histogram() + 
  coord_cartesian(xlim = c(-2, 6))

plot20<-d %>% 
  sample_n(size = 20, replace = TRUE) %>% 
  ggplot() + 
  aes(x = x) + 
  geom_histogram() + 
  coord_cartesian(xlim = c(-2, 6))

plot10 | plot20


# Sampling histogram function
sample_histogram <- function(data, sample_size) { 
  ## Arguments: 
  ##  1. data: which is the dataframe to sample from (in this case, `d`)
  ##  2. the number of samples to take 
  ## Returns: 
  ##  A histogram of the data
  
  data %>% 
    sample_n(size = sample_size, replace = TRUE) %>% 
    ggplot() + 
    aes(x = x) + 
    geom_histogram() + 
    coord_cartesian(xlim = c(-2, 6))
} 

hist100<-sample_histogram(data = d, sample_size = 100)
hist500<-sample_histogram(data = d, sample_size = 500)
hist5000<-sample_histogram(data = d, sample_size = 5000)

hist100 | hist500 | hist5000







#Transformation
# x, y, z and mathstat

df <-  data.frame(x = rnorm(n=10000, mean = 2, sd =1),y = rnorm(n=10000, mean = 2, sd =1)
                  ) %>%
  mutate(z = x + y)
df<-cbind(df,s=rnorm(n=10000, mean = 4, sd = sqrt(2)))

dim(df)

plot_histogram1 <- df %>% 
  ggplot() + 
  aes(x = z) + 
  geom_histogram(bins=30) + 
  coord_cartesian(xlim = c(-2, 10))

plot_histogram2 <- df %>% 
  ggplot() + 
  aes(x = s) + 
  geom_histogram(bins=30) + 
  coord_cartesian(xlim = c(-2, 10))

plot_histogram1 | plot_histogram2

cat("\n z variable mean: ",mean(df[,3]))
cat("\n s variable mean: ",mean(df[,4]))

cat("\n z variable sdev: ",sd(df[,3]))
cat("\n s variable sdev: ",sd(df[,4]))


# math stat result
d <- data.frame(x = rnorm(n = 10000, mean = 4, sd = sqrt(2))) %>% 
  mutate(
    y = dnorm(x = x, mean = 4, sd = sqrt(2))
  )

plot_histogram <- d %>% 
  ggplot() + 
  aes(x = x) + 
  geom_histogram(bins=30) + 
  coord_cartesian(xlim = c(-2, 10))

x<-d[,1]
y<-d[,2]
plot_density <- d %>% 
  ggplot() + 
  aes(x = x, y = y) + 
  geom_line(color = 'red') + 
  coord_cartesian(xlim = c(-2, 10))

plot_histogram | plot_density

# > Notice a few points: 
#   > 1. We were able to do math at the point that we defined `dnorm(z, ...)`. 
# > 2. It looks like this is pretty normal; but, we've relied on a property that we 
#      haven't fully defined yet, namely that V[X + Y] = V[X] + V[Y] - 2COV[X,Y]. 
# > 3. In this case, we can simply sum the sd of `x` and `y` because V[x] = sigma[x] ^ 2 = 1, 
#      and because these are independent draws (and COV[X,Y] = 0). 


# f

verts <-data.frame(cbind(x=c(0,1,1),y=c(0,0,1)))

ggplot(data=verts,aes(x=x,y=y)) + geom_polygon(fill="green")


#The Cake joint, marginal and conditional

# # code from Oleg
# # demonstrating different ways of visualizing normal distribution
# options(repr.plot.width=20, repr.plot.height=4)
# par(mfrow=c(1,3))
# set.seed(0)   # seed random number generator (RNG) for reproducibility 
# 
# n = 100       # Number of samples on x-axis. Try different values.
# IR = c(-4,4)  # interval range of values for x-axis
# 
# x = seq(IR[1], IR[2], (IR[2]-IR[1])/n); y = dnorm(x, 0, 1)
# plot(x, y, xlim=c(-4,4), main = 'x ~ equispaced', cex=2)
# lines(x, y, col='blue', xlim=IR, type='l')
# rug(x); rug(y, side=2); grid()
# 
# x = runif(n, IR[1], IR[2]); y = dnorm(x, 0, 1)
# plot(x,y, main='x ~ uniform(..)', cex=2, xlim=IR)
# lines(x[order(x)], y[order(x)], col='red')
# rug(x); rug(y, side=2); grid()
# 
# x = rnorm(n, 0, 1); y = dnorm(x, 0, 1)
# plot(x,y, main='x ~ Norm(..)', cex=2, xlim=IR)
# lines(x[order(x)], y[order(x)], col='darkgreen')
# rug(x); rug(y, side=2); grid()
