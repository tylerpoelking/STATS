# ----------------------------------------------------------
# File:  dye_yield.R
# Description: fits a Bayesian variance component model
#              to dye yield data using JAGS
# Data:  die_yield_data.txt
# Author:  Kate Calder (calder@stat.osu.edu)
# Course:  STAT 3303 
# ----------------------------------------------------------

# NOTE:  see lecture notes for instructions on 
#        installing JAGS

library(rjags)
library(ggplot2)

set.seed(2343263)

# -------------
# Data set-up
# -------------
# Read in the dye data
dyeData <- read.table("~/Desktop/All Stuff/School Stuff/STATS/3303/Code From Class/JAGS-dye_yield/dye_yield_data.txt",header=T,sep='\t')

# number of batches
nBatches <- dim(dyeData)[2] 

# number of samples of each batch
nSamples <- dim(dyeData)[1]    

# -------------
# JAGS Set-up
# -------------

# create a data list
dataList <- list("y" = as.matrix(dyeData),
                 "nBatches" = nBatches,
                 "nSamples" = nSamples)

# list of parameters to be monitored  
parameters <- c("theta", 
                "tau2WITH", 
                "tau2BTW", 
                "mu") 

# set initial values
initsValues <- list("theta" = 1500, 
                    "tau2WITH" = 1, 
                    "tau2BTW" = 1, 
                    "mu" = rep(1500, nBatches))

# number of iteration for "tuning" 
adaptSteps <- 5000 

# number of iterations for "burn-in" 
burnInSteps <- 5000   

# number of chains to run
nChains <- 2          

# total number of iterations to save
numSavedSteps <- 10000           

# "thinning" (1 = keep every interation)
thinSteps <- 1                  

# iterations per chain
ITER <- ceiling((numSavedSteps * thinSteps )/ nChains) 

# -------------
# Run JAGS
# -------------

# create, initialize, and adapt the model
jagsModel <- jags.model("~/Desktop/All Stuff/School Stuff/STATS/3303/Code From Class/JAGS-dye_yield/dye_yield_model.txt", 
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

# create, initialize, and adapt the model
# approximate the posterior distribution 
# of the proportion of the total variance 
# due to between batch variation

mcmcChainDF$sigma2BTW <- 1/mcmcChainDF$tau2BTW
mcmcChainDF$sigma2WITH <- 1/mcmcChainDF$tau2WITH

mcmcChainDF$propVar <- mcmcChainDF$sigma2BTW /
    (mcmcChainDF$sigma2BTW +
         mcmcChainDF$sigma2WITH)

par(ask = F)
ggplot( mcmcChainDF, aes( x = propVar )) +
    geom_density( color = "red") +
    geom_histogram( aes(y = ..density.. ),
                    alpha = .3) 

mean( mcmcChainDF$propVar )
sd( mcmcChainDF$propVar )



