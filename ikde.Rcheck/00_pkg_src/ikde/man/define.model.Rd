% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/define.model.R
\name{define.model}
\alias{define.model}
\title{Define Stan model}
\usage{
define.model(data, parameters, model, transformed.data = list(),
  transformed.parameters = list())
}
\arguments{
\item{data}{A list of data passed to the Stan program. Should be of the form list(data.name = list(data.type, data.object)).}

\item{parameters}{A list of parameters used in the Stan program. Should be of the form list(parameter.name = parameter.type).}

\item{model}{A list describing the Stan model. Should be a list with components "priors" and "likelihood".}

\item{transformed.data}{A list describing data transformations for the Stan program to perform. Should be of the form list(variable.name = list(variable.type, variable.expression)).}

\item{transformed.parameters}{A list describing parameter transformations for the Stan program to perform. Should be of the form list(variable.name = list(variable.type, variable.expression)).}
}
\value{
Returns an ikde.model object with the following elements
\item{data}{A list of data passed to the Stan program}
\item{transformed.data}{A list describing data transformations for the Stan program to perform}
\item{parameters}{A list of parameters used in the Stan program}
\item{transformed.parameters}{A list describing parameter transformations for the Stan program to perform}
\item{model}{A list describing the Stan model}
\item{stan.code}{Stan code for the model}
\item{stan.data}{Data passed to Stan for estimation}
\item{stan.dso}{DSO for Stan model, allows Stan to run model without recompilation}
\item{built}{Boolean indicating whether the model has been built}
\item{density.variable}{List containing two elements: "name" of the variable on which density estimation should be performed on, and "value" indicating the point at which density should be estimated}
}
\description{
Defines Stan model and stores input data
}
\details{
Defines inputs to be used for building and eventually fitting Stan model.
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

}
