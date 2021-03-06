% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluate.statement.R
\name{evaluate.statement}
\alias{evaluate.statement}
\title{Evaluate sampling statement from Stan program}
\usage{
evaluate.statement(statement, ikde.model, eval.point)
}
\arguments{
\item{statement}{A string containing a sampling statement}

\item{ikde.model}{An object of class ikde.model, which has been built}

\item{eval.point}{A list of parameter names and the point to evaluate the statement}
}
\value{
A real number indicating value of the log-density of the statement at the evaluation point
}
\description{
Evaluate sampling statement from Stan program
}
\details{
Parses the given sampling statement and evaluates it at the specified
evaluation point. The ikde.model object and eval.point object are needed
to resolve variable values in the statement.
}
\examples{
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
sum(dnorm(y, mean = X \%*\% eval.point$beta, sd = sqrt(eval.point$sigma_sq), log = TRUE))
# [1] -4178.641
  
}
