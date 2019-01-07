library(ikde)

rm(list = ls())

data("prostatic.nodes")

y = prostatic.nodes$y
X = cbind(log(prostatic.nodes$X.1), prostatic.nodes$X.3, prostatic.nodes$X.5)

data = list(N = list("int<lower=1>", nrow(X)),
            k = list("int<lower=1>", ncol(X)),
            X = list("matrix[N,k]", X),
            y = list("int[N]", y))
parameters = list(beta = "vector[k]")
transformed.parameters = list(z = c("vector[N]", "z = X * beta"))
model = list(priors = "beta ~ normal(0, 10)",
             likelihood = "y ~ bernoulli(Phi(z))")

ikde.model = define.model(data, parameters, model, transformed.parameters = transformed.parameters)
ikde.model = build.model(ikde.model)