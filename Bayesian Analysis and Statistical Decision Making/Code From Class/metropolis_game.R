# ----------------------------------------------------------
# File:  metropolis_game.R
# Description: implements the metropolis game 
#              played in class                
# Author:  Kate Calder (calder@stat.osu.edu)
# Course:  STAT 3303 
# ----------------------------------------------------------

library(ggplot2)

# -------------
# Set-up
# -------------

# store players as a factor variable
# so that they stay in order in the bar plot

namesPlayers <- c("P1", "P2", "P3", "P4", "P5")

players <- factor(namesPlayers, 
                  levels = namesPlayers)

# number of players
nPlayers <- length(players)

# function to return the index of players
lookPlayer <- function(name) return((1:nPlayers)[players == name])

# original numbers given to players
numTrue <- c(1, 8, 4, 2, 4)

# proportion of the total assigned to 
# each player
propTrue <- numTrue / sum(numTrue)

# create a barplot of the
# proportions
propDF <- data.frame(propTrue = propTrue, players = players)
ggplot(propDF, aes( x = players, y = propTrue) ) + 
  geom_col()

# calculate the move probabilities
# left is column 1
# right is column 2
moveProb <- matrix(NA, nPlayers, 2)
moveProb[1, ] <- c( numTrue[nPlayers], numTrue[2] ) / numTrue[1]
for(i in 2:(nPlayers-1))
  {
    moveProb[i, ] <- c( numTrue[i-1] , numTrue[i+1] ) / numTrue[i]
  }
moveProb[nPlayers, ] <- c( numTrue[nPlayers-1], numTrue[1] ) / numTrue[nPlayers]

# function to play game
play <- function(curr, players, nPlayers, moveProb, lookPlayer)
          {
            # get index of the current player  
            currInd <- lookPlayer(curr)   
            # pick a move direction at random
            moveDirection <- sample(c(1,2), 1)
            # move or don't move
            if(moveDirection == 1)
            {
              propInd <- ifelse( currInd == 1, 
                                  nPlayers, 
                                  currInd - 1 )
            }else{
              propInd <- ifelse( currInd == nPlayers, 
                                  1, 
                                  currInd + 1 )
            }
            if(runif(1,0,1) < moveProb[currInd, moveDirection]) 
            {
              return(players[propInd])
            }else{
              return(players[currInd])
            }
          }

# -------------
# Play the game!
# -------------

# number of interations to play
ITER <- 500

# create a place to store the who has the ball
pathSave <- factor(rep(NA, ITER), 
                    levels = namesPlayers)

# who should start
pathInit <- namesPlayers[1]

# assign the initial person to pathSave
pathSave[1] <- pathInit

# run the rest
for(k in 2:ITER)
  {
    pathSave[k] <- play(pathSave[k-1], 
                         players,
                         nPlayers,
                         moveProb, 
                         lookPlayer)
  }


# summarize the results of the game

propApprox <- table(pathSave)/ITER 

resultsDF <- data.frame(Type = rep(c("True", "Approx"), each = nPlayers),
                        Proportion = c(propTrue, propApprox),
                        Player = factor(rep(as.character(players), 2), 
                                        levels = namesPlayers))

ggplot(resultsDF, aes(x = Player, y = Proportion, fill = Type)) +
         geom_col(position = "dodge")

# -------------
# Some questions
# -------------

# What happens if we play more rounds (iterations)?

# How many rounds do we need to ge a good approximation?

resultsDF2 <- data.frame(iteration = 1:ITER, player = pathSave)

ggplot(resultsDF2, aes( x = iteration, y = as.numeric(player))) +
  geom_path() +
  scale_y_discrete("player", limit = 1:5, label = levels(players)) 
  
# What if we start with a different person?
