##correlation power analysis 
#examine past research and use that to inform power 
#past study, N = 100, found r = .35 
#how many people should you use to get an 80% power

library(pwr)
pwr.r.test(r=.35, power=.80, alternative ="two.sided")

#you need 61 people to detect an r = .35 
#flawed approach; .35 is a sample correlation not the population correlation 
#could get a sample correlation of .35 from something like a population correlation of .02
#would vastly underestimate the number of participants needed 

##Safegaurd Power Analysis protects from this, treats r=.35 as an estimate 

psych::r.con(r=.35,n=100)

#correlation of .35 could have been created by a value as low as, so enter that in the calculation 
pwr.r.test(r =.1649, power=.80, alternative="two.sided")

#need an n of 285 actually, not 61 

##NEW research area, power based on a guess 
#always assume small effect size of .1 

pwr.r.test(r=.1, power=.8, alternative="two.sided")

#need 781 people minimum 

#practice, past study was r=.41 and N = 70 

psych::r.con(r=.41,n=70)

pwr.r.test(r=.1937, power=.8, alternative="two.sided")

#need 206 