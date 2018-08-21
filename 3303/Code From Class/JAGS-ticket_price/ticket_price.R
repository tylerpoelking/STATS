# ----------------------------------------------------------
# File:  ticket_price.R
# Description: refits a normal model (unknown mean 
#              and variance) using JAGS
# Data:  ticket_price.txt
# Author:  Kate Calder (calder@stat.osu.edu)
# Course:  STAT 3303 
# ----------------------------------------------------------

# NOTE:  see lecture notes for instructions on 
#        installing JAGS

library(rjags)
library(ggplot2)
library(grid)
library(gridExtra)

set.seed(2293423)

# -------------
# Data set-up
# -------------

# read in data
priceData <- read.table("ticket_price.txt", header=T)
price <- priceData$price
n <- length(price)

# hyperparameters
mu_0 <- 0
tau2_0 <- 1000
alpha <- 3
beta <- 30

# -------------
# JAGS Set-up
# -------------

# create a data list
dataList <- list("price" = price,
                 "n" = n,
                 "mu_0" = mu_0,
                 "tau2_0" = tau2_0,
                 "alpha" = alpha,
                 "beta" = beta)

# list of parameters to be monitored  
parameters <- c("theta", 
                "prec")

# set initial values
initsValues <- list("theta" = 100, 
                    "prec" = 1/100)

# number of iteration for "tuning" 
adaptSteps <- 5000 

# number of iterations for "burn-in" 
burnInSteps <- 5000   

# number of chains to run
nChains <- 2          

# total number of iterations to save
numSavedSteps <- 5000           

# "thinning" (1 = keep every interation)
thinSteps <- 1                  

# iterations per chain
ITER <- ceiling((numSavedSteps * thinSteps )/ nChains) 

# -------------
# Run JAGS
# -------------

# create, initialize, and adapt the model
jagsModel <- jags.model("ticket_price_model.txt", 
                        data = dataList, 
                        inits = initsValues, 
                        n.chains = nChains, 
                        n.adapt = adaptSteps)

# burn-in the algorithm
update(jagsModel, 
       n.iter = burnInSteps)

# run algorithm to get interations for inference
codaSamples <- coda.samples(jagsModel, 
                            variable.names = parameters, 
                            n.iter = ITER, 
                            thin = thinSteps)

# -------------
# Look at posterior samples
# -------------

# make a dataframe with the posterior samples
mcmcChainDF <- data.frame(as.matrix(codaSamples, 
                                    iters = T, 
                                    chains = T))

# create a vector with the variable names
varNames <- names(mcmcChainDF)[3:(dim(mcmcChainDF)[2])]

# number of variables
nVars <- length(varNames)

mcmcChainDF$CHAIN <- as.factor(mcmcChainDF$CHAIN)

# construct trace plots
par(ask = T)
  for( i in 1:nVars )
  {
    print(ggplot(mcmcChainDF, 
                 aes( x = ITER, 
                      y = mcmcChainDF[ ,varNames[i]])) +
            geom_line(aes(color = CHAIN)) + 
            labs(y = varNames[i]))
    flush.console()
  }

#get posterior samples of sigma2 = 1/prec
mcmcChainDF$sigma2 <- 1/mcmcChainDF$prec

# plot the posterior density of theta and sigma2

p_theta <- ggplot(mcmcChainDF, 
                  aes(x = theta)) +
              geom_density( color = "red") +
              geom_histogram( aes(y = ..density.. ),
                  alpha = .3) +
              labs( main = "theta" )
           
p_sigma2 <- ggplot(mcmcChainDF, 
                  aes(x = sigma2)) +
            geom_density( color = "red") +
            geom_histogram( aes(y = ..density.. ),
                  alpha = .3) +
            labs( main = "sigma2" )

par( ask = F )
grid.arrange(p_theta, p_sigma2, ncol = 1)

# how would you get samples from the posterior 
# predictive distribution?
