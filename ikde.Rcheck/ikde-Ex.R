pkgname <- "ikde"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "ikde-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('ikde')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("build.model")
### * build.model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: build.model
### Title: Build Stan model
### Aliases: build.model

### ** Examples





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("build.model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("create.restricted.models")
### * create.restricted.models

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: create.restricted.models
### Title: Creates restricted models for IKDE
### Aliases: create.restricted.models

### ** Examples





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("create.restricted.models", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("define.model")
### * define.model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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
                   sigma_sq = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma_sq ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))

ikde.model <- define.model(data, parameters, model)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("define.model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evaluate.expression")
### * evaluate.expression

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evaluate.expression", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evaluate.likelihood")
### * evaluate.likelihood

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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
                   sigma_sq = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma_sq ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))

ikde.model <- define.model(data, parameters, model)

eval.point <- list(beta = c(1, 2, 3, 4), sigma_sq = 5)

# These results match:
evaluate.likelihood(ikde.model, eval.point)
sum(dnorm(y, X %*% eval.point$beta, eval.point$sigma_sq, log = TRUE))
# [1] -1054.093
 



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evaluate.likelihood", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evaluate.marginal.likelihood")
### * evaluate.marginal.likelihood

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: evaluate.marginal.likelihood
### Title: Stan model marginal likelihood evaluation
### Aliases: evaluate.marginal.likelihood

### ** Examples





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evaluate.marginal.likelihood", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evaluate.posterior")
### * evaluate.posterior

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: evaluate.posterior
### Title: Stan model posterior evaluation
### Aliases: evaluate.posterior

### ** Examples





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evaluate.posterior", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evaluate.priors")
### * evaluate.priors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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
                   sigma_sq = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma_sq ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))

ikde.model <- define.model(data, parameters, model)

eval.point <- list(beta = c(1, 2, 3, 4), sigma_sq = 5)

# These results match:
evaluate.priors(ikde.model, eval.point)
sum(dnorm(eval.point$beta, 0, 10, log = TRUE), 
    invgamma::dinvgamma(eval.point$sigma_sq, 1, 1, log = TRUE))
# [1] -16.45497
  



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evaluate.priors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("evaluate.statement")
### * evaluate.statement

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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
                   sigma_sq = "real<lower=0>")
model <- list(priors = c("beta ~ normal(0, 10)",
                         "sigma_sq ~ inv_gamma(1, 1)"),
              likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))

ikde.model <- define.model(data, parameters, model)

statement <- ikde.model$model$likelihood[1]
eval.point <- list(beta = c(1, 2, 3, 4), sigma_sq = 5)

# These results match:
evaluate.statement(statement, ikde.model, eval.point)
sum(dnorm(y, mean = X %*% eval.point$beta, sd = sqrt(eval.point$sigma_sq), log = TRUE))
# [1] -4178.641
  



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("evaluate.statement", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit.model")
### * fit.model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit.model
### Title: Fits Stan model
### Aliases: fit.model

### ** Examples





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit.model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gibbs.lm")
### * gibbs.lm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gibbs.lm
### Title: Linear model Gibbs sampling
### Aliases: gibbs.lm

### ** Examples

data(lm.generated)

X <- lm.generated$X
y <- lm.generated$y

gibbs.fit <- gibbs.lm(X, y,
                      priors = list(beta.prior.mean = rep(0, 4),
                                    beta.prior.var = 100 * diag(4),
                                    sigma.sq.prior.shape = 1,
                                    sigma.sq.prior.rate = 1))

print(apply(gibbs.fit$samples$beta, 2, mean)) # [1] 3.181184 1.643960 4.480879 1.213804
print(mean(gibbs.fit$samples$sigma.sq)) # [1] 97.52314
print(gibbs.fit$log.marginal) # [1] -389.001




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gibbs.lm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("stan.multiply")
### * stan.multiply

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
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




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("stan.multiply", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
