library(mlbench)
library(Hmisc)
library(caret)

n <- 100
p <- 40
sigma <- 1

set.seed(10)
sim <- mlbench.friedman1(n, sd = sigma)
colnames(sim$x) <- c(paste("real", 1:5, sep = ""),
                     paste("bogus", 1:5, sep = ""))
bogus <- matrix(rnorm(n * p), nrow = n)
colnames(bogus) <- paste("bogus", 5+(1:ncol(bogus)), sep = "")
x <- cbind(sim$x, bogus)
y <- sim$y
normalization <- preProcess(x)
x <- predict(normalization, x)
x <- as.data.frame(x)

dataset = cbind(x, y)

save(dataset, file = "data/mlbenchFriedman1.rda")
