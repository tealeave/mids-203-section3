library(moments)

setwd("C:/Berkeley/DataSci203_Spring_2025/temp203/instructor_central/live_session/unit_07")

load('GSS_w203.RData')

# Comparing Groups R Exercise

# The General Social Survey (GSS) is one of the longest running and 
# extensive survey projects in the US.  The full dataset includes over 
# 1000 variables spanning demographics, attitudes, and behaviors.  
# The file `GSS_w203.RData` contains a small selection of a variables from the 2018 GSS.  (2022 latest release)
# 
# To learn about each variable, you can enter it into the search bar at the [GSS data explorer](https://gssdataexplorer.norc.org/variables/vfilter)

# You have a set of questions that you would like to answer with a statistical test. 
# **For each question**:
#   
# 1. Choose the most appropriate test.
# 2. List and evaluate the assumptions for your test.
# 3. Conduct your test.
# 4. Discuss statistical and practical significance.
# 
# ## We will explore 6 Questions

###############################################
# - 1. Do Americans spend more evenings with neighbors or with relatives?

summary(GSS$socrel)
summary(GSS$socommun)
table(as.numeric(GSS$socrel), GSS$socrel)
table(as.numeric(GSS$socommun), GSS$socommun)

more_relatives = sum( as.numeric(GSS$socrel) < as.numeric(GSS$socommun), na.rm=T)
trials = sum( as.numeric(GSS$socrel) < as.numeric(GSS$socommun) | as.numeric(GSS$socrel) > as.numeric(GSS$socommun), na.rm=T)
more_relatives/trials

binom.test(more_relatives , trials)

#r = (data in favor - data against)/(total data) 

# The two value  correlation
r<-( 1011 - (1323-1011)) / 1323
r

# r>0.5 strong practical significance
# 0.2<r <= 0.5 moderate practical significance
# 0< r <=0.2 weak practical experience
# r = 0 No PS

# - 2. Are Americans with pets happier than Americans without pets?
# Pets
#############################################
GSS$pets = GSS$numpets>0
summary(GSS$pets)


summary(GSS$happy)


table(GSS$happy, as.numeric(GSS$happy))
w<-wilcox.test(as.numeric(GSS$happy)~GSS$pets)
w

# Counting the number of non-pets
# 1. The two samples are independent of one another
# 2. The two populations have equal variance or spread

# The test statistic W is based on the sum of the ranks assigned to the values in one sample, 
# relative to the pooled values from both samples.
# Colloquially, the statistic W measures how high or low the values from one group tend to be in comparison to the other. 
# A higher W suggests that one group consistently has higher values than the other group, while a lower W suggests the opposite.
# 
# High W: Suggests that values from one sample are generally higher in rank than values from the other sample.
# Low W: Suggests that values from the other sample are higher in rank.

# W  The number of times happy for non - pet owner is > happy for pet owners

# W <- 0
# for(i in 1:length(NP)){
#   for(j in 1:length(P)){
#     if(P[j] < NP[i]) W <- W + 1
#   }
# }
# W

#Practic significance for rank data
cor.test(as.numeric(GSS$happy), as.numeric(GSS$pets), method='spearman')

#Use Cohen's thresholds
#Summary of Cohen's Thresholds for Spearman's ρ:
  
# Small Effect: ∣ρ∣≈0.1 (Weak correlation)
# Medium Effect: ∣ρ∣≈0.3 (Moderate correlation)
# Large Effect: ∣ρ∣≈0.5 (Strong correlation)
# Very Strong Effect: ∣ρ∣≥0.7 (Very strong correlation)


# Common language effect
sum(GSS$pets & !is.na(GSS$happy), na.rm=T)
sum(! GSS$pets  & !is.na(GSS$happy) , na.rm=T)

# Total number of pairs
pairings<-sum(GSS$pets & !is.na(GSS$happy), na.rm=T) * sum(! GSS$pets  & !is.na(GSS$happy) , na.rm=T)
pairings

#By multiplying these two sums, you are calculating the total number of possible pairings between people 
#who own pets and people who do not, considering only those who also have valid happiness data.


# number times 
w$statistic/pairings

# The code w$statistic / pairings calculates the ratio of the Wilcoxon test statistic to 
# the number of pairings between people with pets and without pets.

# The result of this calculation gives you a normalized Wilcoxon statistic. 
# Dividing the test statistic by the total number of pairings provides a measure of the average rank 
# difference between the two groups (people with pets and people without pets).

# A larger value of this ratio would suggest a stronger difference in happiness between people with pets and without pets. 
#A smaller value would suggest less difference or that the ranks of happiness scores are more similar between the two groups.

#Closer to 1 larger difference between groups
#Closer to 0 smaller difference

# IF reject H0 can identify which group in the context of the present is greater by comparing group medians 

# - 3. Do Americans with pets watch more or less TV than Americans without pets?
# TV watching with pets
######################################
summary(GSS$tvhours)
length(!is.na((GSS$tvhours[GSS$pets == T])))
length(!is.na((GSS$tvhours[GSS$pets == F])))

hist(GSS$tvhours)
skewness(GSS$tvhours, na.rm=T)

boxplot(GSS$tvhours ~ GSS$pets)

t.test(GSS$tvhours ~ GSS$pets)
?t.test

PractSign <-3.198718 - 3.316742
PractSign

# - 4. Do Americans spend more time emailing or using the web?
#Time email or web
########################################
sum(!is.na(GSS$wwwhr) & !is.na(GSS$emailhr))
hist(GSS$wwwhr)

hist(GSS$wwwhr)
skewness(GSS$wwwhr, na.rm=T)

hist(GSS$emailhr)
skewness(GSS$emailhr, na.rm=T)

t.test(GSS$wwwhr, GSS$emailhr, paired=T)

c(mean(GSS$wwwhr, na.rm=T), mean(GSS$emailhr, na.rm=T))

#  5. - Do economics majors watch more or less tv than computer science majors?
# Compare TV watching Econ or CS majors
############################################

sum(GSS$major1 == 'economics', na.rm=T)
sum(GSS$major1 == 'computer science', na.rm=T)

hist(GSS[GSS$major1 == 'economics',]$tvhours)
hist(GSS[GSS$major1 == 'computer science',]$tvhours)

w<-wilcox.test(GSS[GSS$major1 == 'economics',]$tvhours, GSS[GSS$major1 == 'computer science',]$tvhours)
w


pairings<-sum(GSS$major1 == 'economics' & !is.na(GSS$tvhours), na.rm=T) *
  sum(GSS$major1 == 'computer science' & !is.na(GSS$tvhours), na.rm=T)
pairings

w$statistic/pairings


econ_cs= GSS[GSS$major1 == 'economics' | GSS$major1 == 'computer science',]

cor.test(econ_cs$tvhours, as.numeric(econ_cs$major1), method = 'spearman')


#  6. - Are Americans that own guns or Americans that don't own guns more likely to have pets?
# gun ownersand pets
#####################################################
setwd("C:/Berkeley/DataSci203_Fall_2024/temp_203/instructor_central/live_session/unit_07")
load('GSS_w203.RData')

summary(GSS$owngun)
GSS$pets = GSS$numpets>0
summary(GSS$pets)



GSS$owngun[GSS$owngun == 'refused'] = NA
GSS$owngun = GSS$owngun == 'yes'

# Rows are gun ownership
# columns are pet ownership
tt<-table(GSS$owngun,GSS$pets)
tt

tF<-tt[1,2]/(tt[1,1]+tt[1,2])  
tT<-tt[2,2]/(tt[2,1]+tt[2,2])

# proportion own pet amongst non-gun owners
tF

# proportion own pet amongst gun owners
tT

t.test(GSS$pets ~ GSS$owngun)

tT - tF


