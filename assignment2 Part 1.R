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

#Define likelihood function, input is a vector called param
likelihood <- function(param){
  #param is a vector variable, and a has the value of its first element
  a = param[1]
  #let b have the value of the 2nd element of param 
  b = param[2]
  #let sd have the value of the 3rd element of param
  sd = param[3]
  #predicted value equals a * x + b.
  #pred is a vector, and the nth element in this vector equals 
  #a times the nth element of vector x plus b
  pred = a*x + b
  #calculates the probability density of y 
  #(the likelihood of a single value in y)
  #where y is normally distributed, mean is the estimated y
  #standard deviation is sd
  #returns the logarithm because the result can get too small 
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  # calculates the probability that predicted values are the same as observations
  # Sum of log equals log of products
  sumll = sum(singlelikelihoods)
  return(sumll)   
}

# Example: plot the likelihood profile of the slope a
slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}
# Calculates the likelihood of a range of values of parameter a
slopelikelihoods <- lapply(seq(3, 7, by=.05), slopevalues )
# plot the Likelihood for a range of parameter values of the slope parameter a.
plot (seq(3, 7, by=.05), slopelikelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")

# Prior distribution
# Define Prior distribution function, input is a vector called param
prior <- function(param){
  # a is the first element of param
  a = param[1]
  # b is the 2nd element of param
  b = param[2]
  #sd is the 3rd element of param
  sd = param[3]
  # prior distribution of a is uniform(0, 10), returned as log
  aprior = dunif(a, min=0, max=10, log = T)
  # prior distribution of b is normal(mean = 0, sd = 5), returned as log
  bprior = dnorm(b, sd = 5, log = T)
  # prior distribution of sd is uniform(0, 30), returned as log
  sdprior = dunif(sd, min=0, max=30, log = T)
  # returns the sum of log of the three prior distributions (i.e. log of product)
  return(aprior+bprior+sdprior)
}

# Defines posterior function, input is the vector param with a, b and sd
posterior <- function(param){
  #returns the log of product of likelihood and prior (i.e. the sum of log)
  return (likelihood(param) + prior(param))
}

######## Metropolis algorithm ################

#Define a proposal function with a parameter vector containing a, b, sd
proposalfunction <- function(param){
  #generates random deviates from the old parameters. 
  # sd for a is 0.1, sd for b is 0.5, sd for sd is 0.3
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

#Define run_metropolisis_MCMC as a function. Inputs: startvalue and iterations
#startvalue is a vector containing the initial a, b and sd
#iterations is the number of iterations to run this algorithm
run_metropolis_MCMC <- function(startvalue, iterations){
  #define chain as a matrix where there are iterations + 1 rows and 3 columns
  chain = array(dim = c(iterations+1,3))
  #the first row of chain is the start value
  chain[1,] = startvalue
  #run the iterations. for the ith iteration, 
  for (i in 1:iterations){
    #choose a new parameter value based on the ith row in chain
    #using the proposal function
    proposal = proposalfunction(chain[i,])
    #calculates the acceptance probability. p1/p2 = exp[log(p1)-log(p2)]
    probab = exp(posterior(proposal) - posterior(chain[i,]))
    #If the acceptance probability is smaller than 1, 
    # then the new start value is the proposed value
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      #otherwise, the new start value is the old value (parameters put into the proposal function)
      chain[i+1,] = chain[i,]
    }
  }
  # After finishing iterations, return the matrix
  return(chain)
}

#set start values as a = 4, b = 0, sd = 10
startvalue = c(4,0,10)
#run the Metropolis-Hastings algorithm
chain = run_metropolis_MCMC(startvalue, 10000)

#set burnIn as 5000
burnIn = 5000
#get which rows are duplicates (therefore, not accepted)
# use mean function to get the probability of jumping
# calculates acceptance rate using 1 - the probability of jumping
#discard 1 - 5000 rows of chain because first steps can be biased
acceptance = 1-mean(duplicated(chain[-(1:burnIn),]))

### Summary: #######################
#draw 6 plots in 2 rows and 3 columns
par(mfrow = c(2,3))
#draw the histogram of posterior of a, with 30 bins
#histogram title is Posterior of a. x label is True value = red line
hist(chain[-(1:burnIn),1],nclass=30, main="Posterior of a", xlab="True value = red line" )
#draw the line indicating the mean of a
abline(v = mean(chain[-(1:burnIn),1]))
#draw the line indicating the true a, in red
abline(v = trueA, col="red" )
#draw the histogram of posterior of b, with 30 bins
#histogram title is Posterior of b. x label is True value = red line
hist(chain[-(1:burnIn),2],nclass=30, main="Posterior of b", xlab="True value = red line")
#draw the line indicating the mean of b
abline(v = mean(chain[-(1:burnIn),2]))
abline(v = mean(chain[-(1:burnIn),1]))
#draw the line indicating the true b, in red
abline(v = trueB, col="red" )
#draw the histogram of posterior of sd, with 30 bins
#histogram title is Posterior of sd. x label is True value = red line
hist(chain[-(1:burnIn),3],nclass=30, main="Posterior of sd", xlab="True value = red line")
#draw the line indicating the mean of sd
abline(v = mean(chain[-(1:burnIn),3]) )
#draw the line indicating the true sd, in red
abline(v = trueSd, col="red" )
#plot the value of a generated in each iteration starting after the burn in time
plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of a")
#draw the horizontal line indicating the true value of a, in red
abline(h = trueA, col="red" )
#plot the value of b generated in each iteration starting after the burn in time
plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of b")
#draw the horizontal line indicating the true b, in red
abline(h = trueB, col="red" )
#plot the value of sd generated in each iteration starting after the burn in time
plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of sd")
#draw the horizontal line indicating the true sd, in red
abline(h = trueSd, col="red" )

# for comparison:
summary(lm(y~x))
