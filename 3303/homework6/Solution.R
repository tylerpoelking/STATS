set.seed(23422432)

library(rjags)

library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)

# read in the data and calculate some
# summaries
salaryData <- read.table( "~/Desktop/All Stuff/School Stuff/STATS/3303/homework6/salaries.txt", header=T )
salary <- salaryData$Salary
dept <- salaryData$Dept
deptNames <- levels( dept )
deptNumeric <- as.numeric( dept )

nFac <- length( salary )
D <- length( unique( dept ) )

# create objects for JAGS
dataList <- list( "salary" = salary,
                  "nFac" = nFac,
                  "D" = D,
                  "dept" = deptNumeric)

# list of parameters to be monitored  
parameters <- c( "theta", 
                 "sigma2",
                 "mu",
                 "tau2")

# set initial values
initsValues <- list( "theta" = rep(100, D), 
                     "sigma2" = 100,
                     "mu" = 100,
                     "tau2" = 100)

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
ITER <- ceiling( (numSavedSteps * thinSteps) / nChains ) 

# -------------
# Run JAGS
# -------------

# create, initialize, and adapt the model
jagsModel <- jags.model( "~/Desktop/All Stuff/School Stuff/STATS/3303/homework6/Salaries_Sol.txt", 
                         data = dataList, 
                         inits = initsValues, 
                         n.chains = nChains, 
                         n.adapt = adaptSteps )

