# ----------------------------------------------------------
# File:  prior_sensitivity.R
# Description: illustrates the influence of the prior on the
#              posterior using Columbus temperature data
#              see course notes for details
# Author:  Kate Calder (calder@stat.osu.edu)
# Course:  STAT 3303 
# ----------------------------------------------------------

# Read in the temperature data
temp.data <- read.table("temperature.txt", header=T)
temp <- temp.data$Temp
N <- length(temp)

# -------------
# Model Set-up
# -------------

# assumed standard deviation of the data
sigma.y <- 2      

# prior mean
prior.mean <- 26    

# prior standard deviation
prior.sd <- 2         

# -------------
# Examine how the posterior distribution 
# changes as the number of data points increases
# -------------

par(mfrow = c(1,1), ask=T)
for(n in 1:N)
	{
	  # select the first n measurements
      y <- temp[1:n]    				
	  
	  # calculate posterior sd
      post.sd <- sqrt( 1/(( 1/prior.sd^2) +
      				(n/sigma.y^2)))     
      				
      # calculate posterior mean
      post.mean <- post.sd^2 * ((prior.mean/prior.sd^2) + 
      					(n*mean(y)/sigma.y^2))  			
      #construct plots
      x <- seq(15,50,length=500)
      plot(x,dnorm(x, prior.mean, prior.sd), type='l', col="red", 
				ylim=c(0, dnorm(post.mean, post.mean, post.sd)), 
				xlim = range(x), 
				xlab="temperature", 
				ylab="",
				main=paste("n = ",n))
      lines(x, dnorm(x, post.mean, post.sd), col="blue")              
      points(y, rep(0,n), col="green", pch=19)
      abline(v= prior.mean, col="red")
      abline(v= post.mean, col="blue")
      abline(v= mean(y), col="green")
      legend(40, dnorm(post.mean,post.mean,post.sd),
      			c("prior","data","posterior"),
				col=c("red","green","blue"), text.col="black",lty=1)
	}
      