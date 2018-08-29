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
data <- data.frame(Field=c(1,2,3,4,5,6,7), Number.Planted=c(50,42,37,45,45,50,41), Number.Emerged=c(25,23,16,21,28,30,21))
y <- data$Number.Emerged
numPlanted <- data$Number.Planted
n <- length(y)

p <- y/numPlanted

print(n)

# hyperparameters
#mu_mu <- 0
#mu_sigma2 <- 4
#sigma2_a <- 0
#sigma2_b <- 4

# -------------
# JAGS Set-up
# -------------

# create a data list
dataList <- list("y" = y,
                 "n" = n,
                 'p' = p,
                 "numPlanted" = numPlanted)

# list of parameters to be monitored  
parameters <- c("theta", "mu_0", "sigma2")

# set initial values
initsValues <- list("theta" = rep(.5, n), "mu_0" = 0, "sigma2" = 2)

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
jagsModel <- jags.model("seeds_model.txt", 
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


mcmcChainDF$prec <- 1/mcmcChainDF$sigma2

p_mu_0 <- ggplot(mcmcChainDF, 
                  aes(x = mu_0)) +
    geom_density( color = "red") +
    geom_histogram( aes(y = ..density.. ),
                    alpha = .3) +
    labs( main = "mu" )

p_sigma2 <- ggplot(mcmcChainDF, 
                   aes(x = sigma2)) +
    geom_density( color = "red") +
    geom_histogram( aes(y = ..density.. ),
                    alpha = .3) +
    labs( main = "sigma2" )

par( ask = F )
grid.arrange(p_mu_0, p_sigma2, ncol = 1)

# how would you get samples from the posterior 
# predictive distribution?
