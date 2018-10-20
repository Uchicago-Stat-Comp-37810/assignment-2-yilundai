#True A has a value 5
trueA <- 5
#True B has a value 0
trueB <- 0
#True Standard Deviation is 10
trueSd <- 10
#Sample size is 31
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB + rnorm(n=sampleSize,mean=0,sd=trueSd)

#Plot x against y, and the plot title is "Test Data"
plot(x,y, main="Test Data")

#Define a function called likelihood, there is a single input called param
likelihood <- function(param){
  #param is a vector variable, and a has the value of its first element
  a = param[1]
  #let b have the value of the 2nd element of param 
  b = param[2]
  #let sd have the value of the 3rd element of param
  sd = param[3]
  #predicted value equals a * x + b
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")
