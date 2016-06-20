# Saw this game when playing pick-up soccer with some Korean friends.
# In order to pick teams each person would show either the palm or back of 
# their hand at the same time as the others, similar to "Rock, Paper, Scissors."
# Then they look to see how many people have their palm or back showing.
# If the group is evenly divided then the game is over and teams are decided.
# Otherwise they continue the game until teams are decided.

handgame <- function(x) {
    # Kill program if input isn't even
    if ((x/2) %% 1 != 0) {
        return ('Not even')
    }
    limit <- 1e5
    count <- 0
    check <- 0
    B <- 10000
    
    # Expected tries is inverse of probability that a random combination
    # of 0's and 1's in a group of x objects will be half 0's and half 1's
    expected <- (2^x)/choose(x, x/2)
    
    # Repeat game B times
    results <- replicate(B, {while (count < limit) {
        while (2 * check != x) {
            check <- sum(sample(c(0,1), x, replace = TRUE))
            count <- count + 1
        }
        return (count)
        count <- 0
        check <- 0
    }
    }
    )
    histdata <- hist (results, breaks = seq(from = min(results), to = max(results)), 
          xlab = "Games", 
          ylab = "Frequency", 
          main = paste("Games Needed to Succeed Given", x,"People"),
          xlim = c(0,max(results)),
          col = "blue",
          right = TRUE)
    abline(v = expected, col = "red")
    abline(v = median(results), col = "green")
    legend(x = "topright", 
        c(paste("Expected Games =", round(expected, 2)), 
            paste("Median Games =", median(results))), 
        col = c("red", "green"),
        lty = 1,
        lwd = 1)
}
