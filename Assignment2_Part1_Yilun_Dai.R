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

######## Compare Outputs ################
compare_outputs<- function (iterations) {
  burnIn <- 0.5 * iterations
  for(i in 1:10){
    startvalue = c(runif(1, 0, 10),runif(1, 0, 5),runif(1, 0, 20))
    chain <- run_metropolis_MCMC(startvalue, iterations)
    a <- chain[-(1:burnIn),1]
    mean_a <- mean(a)
    sd_a <- sd(a)
    print(c("mean(a) = ", mean_a))
    print(c("sd(a) = ", sd_a))
  }
}

compare_outputs(1000)
compare_outputs(10000)
compare_outputs(100000)

### Summary: #######################


trueParam = c(trueA, trueB, trueSd)
MCMC_graph(chain, burnIn, trueParam)


# for comparison:
summary(lm(y~x))
