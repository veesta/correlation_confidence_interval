#calculate the correlation confidence interval 

psych::r.con(r=.21,n=100)


#0.01416621 0.39031834

#prior handout we showed that r=.21 was significant meaning that the population correlation was unlikely to be 0
#just showed that the lower bound of this CI could be .01 though which is garbage 

#what does the confidence intervale 95CI[.01, .39] mean 

#example data: N = 100,000; create the population data for this confidence interval 
#set.seed is a random # generator 

set.seed(8)
library(learnSampling)
lower.pop <- get_cor_data(.01,100000)
upper.pop <- get_cor_data(.39,100000)

#two data frames with 100,000 rows and 2 columns 
#each row is an individual in the population 

head(lower.pop)
head(upper.pop)

#confirm population correlation in each population 

rho.lower <- cor(lower.pop$x, lower.pop$y)
rho.upper <- cor(upper.pop$x, upper.pop$y)

print(rho.lower)
print(rho.upper)

#create the sampling distribution - 10 000 sample correlations from a population where rho = .01 

ci.lower.bound.correlations <- get_cor_samples(pop.data=lower.pop,n=100, 
                                               number.of.samples = 10000, number.of.decimals = 2)

#View(ci.lower.bound.correlations)

##find the values that bound the middle 95% of the lower bound sampling distribution - middle is most likely 

#first sort data 
ci.lower.bound.correlations.sorted <- sort_samples_by_r(ci.lower.bound.correlations)

#n=10 000, don't want 2.5% from either side so 250 - we want 251 - 9750 

lower.cilowerbound <- ci.lower.bound.correlations.sorted$r[251]
upper.cilowerbound <- ci.lower.bound.correlations.sorted$r[9750]

print(lower.cilowerbound)
print(upper.cilowerbound)

#range of values is -.18 to .21, any value here is likely 
#.21 falls in this range, so the sampling correlation is likely when population correlation is .01 

library(ggplot2)

#graph of possible sampling correlations when population correlation is .01
lower.bound.hist <-qplot(r, data=ci.lower.bound.correlations, binwidth=.05)
print (lower.bound.hist)


#do exact same thing for upper bound distribution p. 4 


ci.upper.bound.correlations <- get_cor_samples(pop.data=upper.pop,n=100, 
                                               number.of.samples = 10000, number.of.decimals = 2)

ci.upper.bound.correlations.sorted <- sort_samples_by_r(ci.upper.bound.correlations)

lower.ciupperbound <- ci.upper.bound.correlations.sorted$r[251]
upper.ciupperbound <- ci.upper.bound.correlations.sorted$r[9750]

print(lower.ciupperbound)
print(upper.ciupperbound)

upper.bound.hist <-qplot(r, data=ci.upper.bound.correlations, binwidth=.05)

#PLOT BOTH HISTOGRAMS ON SAME GRAPH 

both.hist <- ggplot(ci.upper.bound.correlations,aes(r))
both.hist <- both.hist + geom_histogram(aes(x= ci.upper.bound.correlations$r, y=..count..), fill ="red",binwidth=.05)
both.hist <- both.hist + geom_histogram(aes(x= ci.lower.bound.correlations$r, y=..count..), fill ="blue",binwidth=.05)
print(both.hist)

##Sample Size Impact on N 
#confidence interval becomes more precise/accurate to true population correlation with increasing N 
