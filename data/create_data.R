setwd("~/git/ikde-scripts/data")

rm(list = ls())

#Chib
prostatic.nodes = read.csv("chib_data.csv")
save(prostatic.nodes, file = "../../ikde/data/prostatic.nodes.RData")

#Multivariate linear
set.seed(100)

N <- 100
k <- 4
sd <- 10

X <- cbind(1, matrix(runif(N * (k - 1)), ncol = k - 1))
beta <- runif(k, -5, 5)
y <- X %*% beta + rnorm(N, sd = sd)
y <- c(y)

lm.generated = list(X = X, y = y)
save(lm.generated, file = "../../ikde/data/lm.generated.RData")
