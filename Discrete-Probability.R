library(dplyr)
library(dslabs)
library(gtools) # we use combinations() and permutations() functions from this package
library(tidyverse)


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

# Probability of a King in the first card
kings <- paste("King", suits)
mean(deck %in% kings)

# Permutations and Combinations from library(gtools) package
permutations(3,2)
combinations(3,2)

all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

# Deck of cards again
# To compute all possible ways we can choose two cards when the order matters, we type:
hands <- permutations(52, 2, v = deck) 
first_card <- hands[,1]
second_card <- hands[,2]
kings <- paste("King", suits)
sum(first_card %in% kings) # cases for which the first hand was a King
# To get the conditional probability, we compute what fraction of these have a King in the second card
# Pr(B∣A)=Pr(A and B)/Pr(A)
sum(first_card%in%kings & second_card%in%kings) / sum(first_card%in%kings)
# Or 
mean(first_card%in%kings & second_card%in%kings) / mean(first_card%in%kings)

# Probability of a natural 21 in blackjack: picking an Ace card and a Suit
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
# In the last line, we assume the Ace comes first. This is only because we know the way combination enumerates possibilities and it will list this case first. But to be safe, we could have written this and produced the same answer
mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard))

# Monte Carlo example
hand <- sample(deck, 2)
hand 
# And then check if one card is an Ace and the other a face card or a 10. Going forward, we include 10 when we say face card. Now we need to check both possibilities:
(hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)

blackjack <- function(){
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | 
    (hand[2] %in% aces & hand[1] %in% facecard)
}

B <- 10000
results <- replicate(B, blackjack())
mean(results)

# Birthday Problem
n <- 50
bdays <- sample(1:365, n, replace = TRUE)
duplicated(c(1,2,3,1,4,3,5)) # duplicated() returns TRUE whenever an element of a vector is a duplicate
any(duplicated(bdays))

B <- 10000
same_birthday <- function(n){
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
}
results <- replicate(B, same_birthday(50))
mean(results)

# Say we want to use this knowledge to bet with friends about two people having the same birthday in a group of people. When are the chances larger than 50%? Larger than 75%?
# Let’s create a look-up table. We can quickly create a function to compute this for any group size:
compute_prob <- function(n, B=10000){
  results <- replicate(B, same_birthday(n))
  mean(results)
}
# Using the function sapply, we can perform element-wise operations on any function:
n <- seq(1,60)
prob <- sapply(n, compute_prob)
qplot(n, prob)

# Now let’s compute the exact probabilities rather than use Monte Carlo approximations
# To make the math simpler, instead of computing the probability of it happening, we will compute the probability of it not happening. For this, we use the multiplication rule

# Let’s start with the first person. The probability that person 1 has a unique birthday is 1
# The probability that person 2 has a unique birthday, given that person 1 already took one, is 364/365
# Then, given that the first two people have unique birthdays, person 3 is left with 363 days to choose from
# We continue this way and find the chances of all 50 people having a unique birthday is:
# 1×(364/365)×(363/365)…((365−n+1)/365)
# We can write a function that does this for any number:
exact_prob <- function(n){
  prob_unique <- seq(365,365-n+1)/365 
  1 - prod( prob_unique)
}
eprob <- sapply(n, exact_prob)
qplot(n, prob) + geom_line(aes(n, eprob), col = "red")


# Addition Rule
# We apply the addition rule where = drawing an ace then a facecard and = drawing a facecard then an ace. Note that in this case, both events A and B cannot happen at the same time, so Pr(A and B) = 0

# Pr(ace then facecard) = (4/52)*(16/51)
# Pr(facecard then ace) = (16/52)*(4/51)
# # Pr(ace then facecard | facecard then ace) = (4/52)*(16/51) + (16/52)*(4/51)


# Monty Hall problem
cat("\014")
B <- 10000
monty_hall <- function(strategy){
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick  <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
  switch <- doors[!doors%in%c(my_pick, show)]
  choice <- ifelse(strategy == "stick", stick, switch)
  choice == prize_door
}
stick <- replicate(B, monty_hall("stick"))
mean(stick)
switch <- replicate(B, monty_hall("switch"))
mean(switch)

