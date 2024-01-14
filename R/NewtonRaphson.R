#' The logregNewtonRaphson function performs the Newton-Raphson optimisation
#' to obtain the mle estimates for coefficients in a logistic regression model
#'
#' @param X A model matrix without intercept
#' @param y A response vector of 0s and 1s
#' @param max.iter The maximum number of iteration
#' @param tol A convergence criterion
#'
#' @returns coefficients: vector containing the estimates
#' @returns var: variance-covariance matrix of the estimates
#' @returns iterations: number of iterations required
#'
#' @author Solichatus Zahroh \email{s.solichatuszahroh@students.uu.nl}

logregNewtonRaphson <- function(X, y, max.iter=30, tol=1E-6){

  # we first add the intercept column to the model matrix
  X <- cbind(1, X)

  # we introduce vectors beta and beta.last which will contain the estimates
  # initial values are set to 0 for all coefficients
  beta <- beta.last <- rep(0, ncol(X))

  # we will keep track of the number of iterations using the variable it
  # we set it to 1 for the first iteration
  it <- 1

  # we use a while loop which stops as soons as it (increased after each interaction)
  # exceeds the predefined maximum number
  while (it <= max.iter){

    # p is the initial estimate for the probabilities
    p <- as.vector(1/(1 + exp(-X %*% beta)))

    # W is the matrix containing variances of the observations in y
    # using the current estimates for the probabilities

    W <- diag(p * (1 - p))

    # Var.beta is the variance-covariance matrix for the current estimates of the
    # regression coefficients

    var.beta <- solve(t(X) %*% W %*% X)

    # updating step for beta
    beta <- beta + var.beta %*% t(X) %*% (y - p)

    # check if convergence is reached.
    # if so break forces the while loop to end
    # otherwise beta.last will be updated and it will increased for another iteration
    if (max(abs(beta - beta.last)/(abs(beta.last)
                                   + 0.01*tol)) < tol)
      break
    beta.last <- beta
    it <- it + 1
  }
  # a warning is given in case convergence is not achieved for prespecified number of
  # maximum iterations
  if (it > max.iter)
    warning('maximum iterations exceeded')

  # a list is returned as output with coeffients, variance-covariance matrix of the vector of
  # coeffients and the number of iterations needed
  list(coefficients=as.vector(beta), var=var.beta,
       iterations=it)
}
