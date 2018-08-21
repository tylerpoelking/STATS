# ----------------------------------------------------------
# File:  multinomial_example.R
# Description: election polling example
# Author:  Kate Calder (calder@stat.osu.edu)
# Course:  STAT 3303 
# ----------------------------------------------------------

library(ggplot2)
library(grid)
library(gridExtra)

# -------------
#Survey Data
# -------------

# number of Trump supporters
y1 <- 727   

# number of Cruz supporters
y2 <- 583     

# number who support other candidates
y3 <- 137            

# sample size
n.individs <- y1+y2+y3

# -------------
# Samples from the 
# posterior distribution
# -------------

# Parameters of the posterior distribution of theta
alpha.post <- c(y1+1, y2+1, y3+1)

# Function to sample from a Dirichlet distribution 
rdirichlet <- function(n=1,alpha)
				{
                	k <- length(alpha)
                	x <- rep(0,k)
                	theta <- matrix(0,nrow=n,ncol=k)
                	for(i in 1:n)
                		{
                    		x <- apply(as.matrix(alpha), 1, 
                    				function(shape.x) return(rgamma(1,shape=shape.x,scale=1)))
                    		theta[i,] <- x/sum(x)
                    	}
                	return(theta)
                }

# Draw a random sample from the posterior
rdirichlet(1, alpha.post)

# Draw 1000 random samples from the posterior
post.samples <- rdirichlet(1000, alpha.post)

# -------------
# Posterior summaries
# -------------

# Histograms of the posterior samples of the theta's
df.post.samples <- data.frame(post.samples)
names(df.post.samples) <- c("theta_1", "theta_2", "theta_3")

p1 <- ggplot(df.post.samples, aes( x = theta_1 )) +
  geom_histogram( aes(y = ..density.. ), 
                  fill = "blue", alpha = 0.2)
p2 <- ggplot(df.post.samples, aes( x = theta_2 )) +
  geom_histogram( aes(y = ..density.. ), 
                  fill = "blue", alpha = 0.2)
p3 <- ggplot(df.post.samples, aes( x = theta_3 )) +
  geom_histogram( aes(y = ..density.. ), 
                  fill = "blue", alpha = 0.2)
grid.arrange(p1, p2, p3, ncol = 3)

# Histogram of samples from the posterior 
# distribution of theta_1-theta_2 -- INTERPRETATION?
df.post.diff <- data.frame( post.samples[ , 1] - post.samples[ , 2] )
names(df.post.diff) <- "diff.theta"
ggplot(df.post.diff, aes( x = diff.theta )) +
  geom_histogram( aes(y = ..density.. ), 
                  fill = "blue", alpha = 0.2)

# Posterior probability that theta_1 > theta_2
mean( post.samples[ , 1] > post.samples[ , 2] )    