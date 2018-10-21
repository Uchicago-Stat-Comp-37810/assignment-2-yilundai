#setwd("~/Documents/Documents/Fall2018/StatsComputingA/assignment-2-yilundai")
source("mcmc_functions.R")
trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

plot(x,y, main="Test Data")


# Example: plot the likelihood profile of the slope a

slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")




######## Metropolis algorithm ################



startvalue = c(4,0,10)
chain = run_metropolis_MCMC(startvalue, 10000)

burnIn = 5000
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

### Summary: #######################


trueParam = c(trueA, trueB, trueSd)
MCMC_graph(chain, burnIn, trueParam)


# for comparison:
summary(lm(y~x))
