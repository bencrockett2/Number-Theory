library(dplyr)

# set group size and number of trials
n <- 100
B <- 1000000

# probability of winning game is (n choose n/2) / 2^n
odds <- choose(n, n/2)/(2^n)

# set1 simulates B trials of n people and sees how many have won
# then find distance between winning games to get vector of game lengths
set1 <- replicate(B, sum(sample(c(0,1), n, replace = TRUE)))
gamels1 <- diff(which(set1 == n/2))

# set2 is vector of 0's and 1's, where prob of 1 occurring is "odds"
# then find discance between 1's
set2 <- sample(c(0,1), B, replace = TRUE, prob = c(1-odds, odds))
gamels2 <- diff(which(set2 == 1))

# initialize multiple plots and set x and y maxima
par(mfrow = c(1, 2))
xmax <- 40
ymax <- 13000

# plot histograms of lengths of games
histdata1 <- hist(gamels1, 
                  breaks = seq(from = min(gamels1), to = max(gamels1)),
                  xlim = c(0, xmax),
                  ylim = c(0, ymax))
histdata2 <- hist(gamels2, 
                  breaks = seq(from = min(gamels2), to = max(gamels2)),
                  xlim = c(0, xmax),
                  ylim = c(0, ymax))

# misc code below looking at total trials, e.g. if there are 2000 3-trial
# games, then that's a total of 6000 trials.  I.e. how much "time is spent"
# in a certain length of game during the simulation?

a <- histdata$counts
b <- (1:length(a))
histdata$counts <- b * histdata$counts
plot(histdata)
gamecounts <- a * b
plot(gamecounts)

