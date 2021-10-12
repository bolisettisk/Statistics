library(dplyr)
library(dslabs)


n<- 1000
loss_per_foreclosure <- 200000
p <- 0.2
defaults <- sample(c(0,1), n, replace = TRUE, prob = c(1-p,p))
sum(defaults*loss_per_foreclosure)

cat("\014")
B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0,1), n, replace = TRUE, prob = c(1-p,p))
  sum(defaults*loss_per_foreclosure)
})
mean(losses)

  