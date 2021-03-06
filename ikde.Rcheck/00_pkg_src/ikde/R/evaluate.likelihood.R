#' Stan model likelihood evaluation
#' 
#' Evaluates likelihood of Stan model at specified evaluation point
#' 
#' @param ikde.model An object of class ikde.model which has been built
#' @param eval.point A list of parameter names and the point to evaluate the likelihood
#' 
#' @return A real number indicating value of the log-likelihood at the specified evaluation
#' point
#' 
#' @details 
#' Parses sampling statements in ikde.model$model$likelihood and evaluates them at the specified
#' evaluation point.
#' 
#' @examples
#' data(lm.generated)
#' 
#' X <- lm.generated$X
#' y <- lm.generated$y
#' 
#' data <- list(N = list("int<lower=1>", nrow(X)),
#'              k = list("int<lower=1>", ncol(X)),
#'              X = list("matrix[N, k]", X),
#'              y = list("vector[N]", y))
#' parameters <- list(beta = "vector[k]",
#'                    sigma_sq = "real<lower=0>")
#' model <- list(priors = c("beta ~ normal(0, 10)",
#'                          "sigma_sq ~ inv_gamma(1, 1)"),
#'               likelihood = c("y ~ normal(X * beta, sqrt(sigma_sq))"))
#' 
#' ikde.model <- define.model(data, parameters, model)
#' 
#' eval.point <- list(beta = c(1, 2, 3, 4), sigma_sq = 5)
#' 
#' # These results match:
#' evaluate.likelihood(ikde.model, eval.point)
#' sum(dnorm(y, X %*% eval.point$beta, eval.point$sigma_sq, log = TRUE))
#' # [1] -1054.093
#'  
#' @export

evaluate.likelihood <-
  function(ikde.model, eval.point){
    if (class(ikde.model) != "ikde.model") stop("ikde.model must be of class \"ikde.model\".")
    if (class(eval.point) != "list") stop("eval.point must be a list.")
    
    return(sum(sapply(ikde.model$model$likelihood, evaluate.statement,
                      ikde.model = ikde.model, eval.point = eval.point)))
  }
