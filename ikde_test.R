library(ikde)

rm(list = ls())

data("lm.generated")
X = lm.generated$X
y = lm.generated$y

k = ncol(X)

#Gibbs model first
gibbs.fit = gibbs.lm(X, y,
                     priors = list(beta.prior.mean = rep(0, k),
                                   beta.prior.var = 100 * diag(k),
                                   tau.prior.shape = 1,
                                   tau.prior.rate = 1))

eval.point = list(beta = apply(gibbs.fit$samples$beta, 2, mean),
                  sigma_sq = 1 / mean(gibbs.fit$samples$tau))

#Now Stan model
data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma_sq = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma_sq ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))
ikde.model <- define.model(data, parameters, model)

#Compare log-marginals
#These don't match
# evaluate.marginal.likelihood(ikde.model, 5000, 5000) # [1] -365.8268
# gibbs.fit$log.marginal # [1] -389.0013

#Original sources
#These match Gibbs results from above
# source("gibbs_stan_lm/gibbs_ml.R")
# log.marginal
# source("gibbs_stan_lm/stan_ml_ks_iterative_fit.R")

#Tracking down issues
evaluate.priors(ikde.model, eval.point)
gibbs.fit$log.prior

evaluate.likelihood(ikde.model, eval.point)
gibbs.fit$log.lik
