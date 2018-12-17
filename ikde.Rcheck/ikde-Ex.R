pkgname <- "ikde"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('ikde')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build.model")
### * build.model

flush(stderr()); flush(stdout())

### Name: build.model
### Title: Build Stan model
### Aliases: build.model

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)
ikde.model <- build.model(ikde.model)

cat(ikde.model$stan.code)




cleanEx()
nameEx("create.restricted.models")
### * create.restricted.models

flush(stderr()); flush(stdout())

### Name: create.restricted.models
### Title: Creates restricted models for IKDE
### Aliases: create.restricted.models

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)
eval.point <- list(beta = c(1, 2, 3, 4),
                   sigma = 5)

ikde.model.list <- create.restricted.models(ikde.model, eval.point)
for (restricted.ikde.model in ikde.model.list){
  cat(restricted.ikde.model$stan.code)
  cat("--------------------------------------------------\n")
}




cleanEx()
nameEx("define.model")
### * define.model

flush(stderr()); flush(stdout())

### Name: define.model
### Title: Define Stan model
### Aliases: define.model

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)




cleanEx()
nameEx("evaluate.expression")
### * evaluate.expression

flush(stderr()); flush(stdout())

### Name: evaluate.expression
### Title: Evaluate expression from Stan program
### Aliases: evaluate.expression

### ** Examples

X <- matrix(1:9, nrow = 3)
b <- c(4, 5, 6)

stan.expression <- "(3 + 2) * X * (5 * b)"

# These results match:
evaluate.expression(stan.expression)
print((3 + 2) * X %*% (5 * b))
#      [,1]
# [1,] 1650
# [2,] 2025
# [3,] 2400




cleanEx()
nameEx("evaluate.likelihood")
### * evaluate.likelihood

flush(stderr()); flush(stdout())

### Name: evaluate.likelihood
### Title: Stan model likelihood evaluation
### Aliases: evaluate.likelihood

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)
ikde.model <- build.model(ikde.model)

eval.point <- list(beta = c(1, 2, 3, 4), sigma = 5)

# These results match:
evaluate.likelihood(ikde.model, eval.point)
sum(dnorm(y, X %*% eval.point$beta, eval.point$sigma, log = TRUE))
# [1] -1054.093
  



cleanEx()
nameEx("evaluate.marginal.likelihood")
### * evaluate.marginal.likelihood

flush(stderr()); flush(stdout())

### Name: evaluate.marginal.likelihood
### Title: Stan model marginal likelihood evaluation
### Aliases: evaluate.marginal.likelihood

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)

evaluate.marginal.likelihood(ikde.model) # Only an estimation, may not exactly match presented result
# [1] -368.3207
  



cleanEx()
nameEx("evaluate.posterior")
### * evaluate.posterior

flush(stderr()); flush(stdout())

### Name: evaluate.posterior
### Title: Stan model posterior evaluation
### Aliases: evaluate.posterior

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)
ikde.model <- build.model(ikde.model)
stan.fit <- fit.model(ikde.model)
stan.extract <- rstan::extract(stan.fit)

eval.point <- list(beta = apply(stan.extract$beta, 2, mean),
                   sigma = mean(stan.extract$sigma))

evaluate.posterior(ikde.model, eval.point) # Only an estimation, may not exactly match presented result
# [1] -39.95366




cleanEx()
nameEx("evaluate.priors")
### * evaluate.priors

flush(stderr()); flush(stdout())

### Name: evaluate.priors
### Title: Stan model prior evaluation
### Aliases: evaluate.priors

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)
ikde.model <- build.model(ikde.model)

eval.point <- list(beta = c(1, 2, 3, 4), sigma = 5)

# These results match:
evaluate.priors(ikde.model, eval.point)
sum(dnorm(eval.point$beta, 0, 10, log = TRUE), 
    invgamma::dinvgamma(eval.point$sigma, 1, 1, log = TRUE))
# [1] -16.45497
  



cleanEx()
nameEx("evaluate.statement")
### * evaluate.statement

flush(stderr()); flush(stdout())

### Name: evaluate.statement
### Title: Evaluate sampling statement from Stan program
### Aliases: evaluate.statement

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)
ikde.model <- build.model(ikde.model)

statement <- ikde.model$model$likelihood[1]
eval.point <- list(beta = c(1, 2, 3, 4), sigma = 5)

# These results match:
evaluate.statement(statement, ikde.model, eval.point)
sum(dnorm(y, mean = X %*% eval.point$beta, sd = eval.point$sigma, log = TRUE))
# [1] -1054.093
  



cleanEx()
nameEx("fit.model")
### * fit.model

flush(stderr()); flush(stdout())

### Name: fit.model
### Title: Fits Stan model
### Aliases: fit.model

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

data <- list(N = list("int<lower=1>", nrow(X)),
             k = list("int<lower=1>", ncol(X)),
             X = list("matrix[N, k]", X),
             y = list("vector[N]", y))
parameters <- list(beta = "vector[k]",
                   sigma = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sigma)"))

ikde.model <- define.model(data, parameters, model)
ikde.model <- build.model(ikde.model)
stan.fit <- fit.model(ikde.model)
stan.extract <- extract(stan.fit)

print(apply(stan.extract$beta, 2, mean)) # Only an estimation, may not exactly match presented result
# [1] 3.236087 1.629510 4.496279 1.211404




cleanEx()
nameEx("stan.multiply")
### * stan.multiply

flush(stderr()); flush(stdout())

### Name: %stan*%
### Title: Function to replicate multiplication in Stan
### Aliases: %stan*%

### ** Examples

X <- matrix(1:9, nrow = 3)
b <- c(4, 5, 6)

(3 + 2) * X %stan*% (5 * b)
#      [,1]
# [1,] 1650
# [2,] 2025
# [3,] 2400




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
