library(ikde)

rm(list = ls())

data("prostatic.nodes")

y = prostatic.nodes$y
X = cbind(log(prostatic.nodes$X.1), prostatic.nodes$X.3, prostatic.nodes$X.5)

data = list(N = list(type = "int<lower=1>", dim = 1, value = nrow(X)),
            k = list(type = "int<lower=1>", dim = 1, value = ncol(X)),
            X = list(type = "matrix", dim = "[N, k]", value = X),
            y = list(type = "int", dim = "[N]", value = y))
parameters = list(beta = list(type = "vector", dim = "k"))
transformed.parameters = list(z = list(type = "vector", dim = "N", expression = "z = X * beta;"))
model = list(priors = "beta ~ normal(0, 10);",
             likelihood = "y ~ bernoulli(Phi(z));")

ikde.model = define.model(data, parameters, model, transformed.parameters = transformed.parameters)
ikde.model = build.model(ikde.model)