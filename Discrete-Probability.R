library(dplyr)
library(dslabs)


# Monte Carlo Simulations
beads <- rep(c("red", "blue"), times = c(2,3))
beads
sample(beads, 1)

B <- 10000
events <- replicate(B, sample(beads,1))
tab <- table(events)
tab
prop.table(tab)

#Or 
events_s <- sample(beads, B, replace = TRUE)
tab_s <- table(events_s)
tab_s
prop.table(tab_s)

# Or
mean(events == "blue")
mean(events_s == "blue")

# Combinations and Permutations

number <- "Three"
suit <- "Hearts"
paste(number, suit)
# paste is used to create strings by joining smaller strings
# paste can also be used on vectors and it operates element wise

# The function expand.grid gives us all combinations of entries of two vectors
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

# Create Deck of Cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
deck

cat("\014")
# Probability of a King in the first card
kings <- paste("King", suits)
mean(deck %in% kings)




