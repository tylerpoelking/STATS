# ----------------------------------------------------------
# File:  births.R
# Description: calculates summaries of the posterior distribution
#              of female births in placenta previa pregnancies --
#              see course notes for details
# Author:  Kate Calder (calder@stat.osu.edu)
# Course:  STAT 3303
# ----------------------------------------------------------

library(ggplot2)
library(grid)
library(gridExtra)

# ----------
# Summarizing the exact posterior distribution --
# Beta(438,544)
# ----------

# posterior mean
post.mean <- 0.446     

# posterior sd
post.sd <- 0.016                    	

# posterior quantiles/median (50% quantile)
post.median <- qbeta(.5, 438, 544)      
post.median

# 95% credible interval 
post.interval.95 <- qbeta( c(0.025, 0.975), 
                           438, 544)   	
post.interval.95

# plot the posterior density
theta <- seq(0, 1, length=500)
df.post <- data.frame(theta = theta, post.theta = dbeta(theta, 438, 544))
df.post.summaries <-  data.frame(value = c(post.mean, jitter(post.median, 
                                                             amount = 0.001), 
                                           post.interval.95),
                              type = c("post.mean", "post.median", "95% CI", 
                                       "95% CI"))
ggplot(df.post, aes(x = theta, y = post.theta)) + 
  geom_line() +
  geom_vline(data = df.post.summaries, 
             mapping = aes(xintercept = value, color = type )) +
  ylab("p(theta | y)") + 
  xlim(.35, .55) + 
  labs( color = "summary")

# ----------
# QUESTION:  How much evidence is there that the proportion of 
# female placenta previa births is less than 0.485?
# ----------

theta.sm <- seq(0, 0.485, length=500)
df.area <- data.frame( theta = theta.sm, 
                       post.theta = dbeta( theta.sm, 438, 544) )
ggplot( df.post, aes(x = theta, y = post.theta)) + 
  geom_line() +
  geom_area( data = df.area, 
             mapping = aes( x = theta, y = post.theta),
             alpha = 0.2) +
  ylab("p(theta | y)") + 
  xlim( .35, .55)

# area less that 0.485
pbeta(0.485, 438, 544)  

# ----------
# Approximating the posterior distribution
# of theta
# ----------

# get 1000 draws from the Beta(438,544) distribution
theta.samples <-  rbeta( 1000, 438, 544)

# approximate posterior mean
post.mean.approx <- mean( theta.samples )      
post.mean.approx
post.mean

# approximate posterior sd
post.sd.approx <- sd( theta.samples )          
post.sd.approx
post.sd

# approximate posterior median
post.median.approx <- median( theta.samples )  
post.median.approx
post.median

# approximate 95% credible interval
post.interval.95.approx <- quantile( theta.samples, c(.025, .975) )  
post.interval.95.approx
post.interval.95

# approximate the posterior density function
df.theta.samples <- data.frame( theta = theta.samples)
post.dens <- function(x) dbeta(x, 438, 544)
ggplot(df.theta.samples, aes( x = theta )) +
  geom_histogram( aes(y = ..density.. ), fill = "blue", alpha = 0.2) +
  stat_function( fun = post.dens, lwd = 1)

# Take 2:  larger sample size
theta.samples.big <-  rbeta(100000, 438, 544)
df.theta.samples <- data.frame( theta = theta.samples.big)
ggplot(df.theta.samples, aes( x = theta )) +
  geom_histogram( aes(y = ..density.. ), fill = "blue", alpha = 0.2) +
  stat_function( fun = post.dens, lwd = 1)

# ----------
# Posterior distribution of
# transformations of theta
# ----------

df.theta.tr <- data.frame( theta = theta.samples )
df.theta.tr$log.theta <- log( theta.samples )
df.theta.tr$sex.ratio <- theta.samples / ( 1 - theta.samples )
df.theta.tr$log.odds <- log( df.theta.tr$sex.ratio )

p1 <- ggplot(df.theta.tr, aes( x = theta )) +
        geom_histogram( aes(y = ..density.. ), 
                        fill = "blue", alpha = 0.2)
p2 <- ggplot(df.theta.tr, aes( x = log.theta )) +
        geom_histogram( aes(y = ..density.. ), 
                  fill = "blue", alpha = 0.2)
p3 <- ggplot(df.theta.tr, aes( x = sex.ratio )) +
        geom_histogram( aes(y = ..density.. ), 
                  fill = "blue", alpha = 0.2)
p4 <- ggplot(df.theta.tr, aes( x = log.odds )) +
        geom_histogram( aes(y = ..density.. ), 
                  fill = "blue", alpha = 0.2)
grid.arrange(p1, p2, p3, p4, ncol = 2)
