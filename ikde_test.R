library(ikde)

rm(list = ls())

N = 100
k = 4
sd = 10

X = cbind(1, matrix(runif(N * (k - 1), -10, 10), ncol = k - 1))
beta = runif(k, -5, 5)
y = X %*% beta + rnorm(N, sd = sd)
y = c(y)

data = list(N = list("int<lower=1>", N),
            k = list("int<lower=1>", k),
            X = list("matrix[N, k]", X),
            y = list("vector[N]", y))
parameters = list(beta = "vector[k]",
                  sigma = "real<lower=0>")
model = list(priors = c("beta ~ normal(0, 10)",
                        "sigma ~ inv_gamma(1, 1)"),
             likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model = define.model(data, parameters, model)
ikde.model = build.model(ikde.model)

stan.fit = fit.model(ikde.model, display.output = TRUE)
stan.extract = extract(stan.fit)
