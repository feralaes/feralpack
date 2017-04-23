#' Calculate alpha parameters of Dirichlet distribution.
#' 
#' Function to calculate the \eqn{\alpha} parameters of the Dirichlet distribution
#' based on the method of moments (MoM) using the mean \eqn{\mu} and standard
#' deviation \eqn{\sigma} of the random variables of interest.
#' @param p.mean Vector of means of the random variables.
#' @param sigma Vector of standard deviation of the random variables 
#' (i.e., standar error).
#' @keywords dirichlet distribution; method of moments
#' @section Details:
#' Based on methods of moments. If \eqn{\mu} is a vector of means and 
#' \eqn{\sigma} is a vector of standard deviations of the random variables, then
#' the second moment \eqn{X_2} is defined by \eqn{\sigma^2 + \mu^2}. Using the
#' mean and the second moment, the \eqn{J} alpha parameters are computed as follows
#' \deqn{\alpha_i = \frac{(\mu_1-X_{2_{1}})\mu_i}{X_{2_{1}}-\mu_1^2}}
#' for \eqn{i = 1, \ldots, J-1}, and
#' \deqn{\alpha_J = \frac{(\mu_1-X_{2_{1}})(1-\sum_{i=1}^{J-1}{\mu_i})}{X_{2_{1}}-\mu_1^2}}
#' 
#' @references
#' \enumerate{
#' \item Fielitz BD, Myers BL. Estimation of parameters in the beta distribution. 
#' Dec Sci. 1975;6(1):1–13. 
#' \item Narayanan A. A note on parameter estimation in the multivariate beta 
#' distribution. Comput Math with Appl. 1992;24(10):11–7. 
#' }
#' @return alpha Alpha parameters of dirichlet distribution
#' @examples
#' \dontrun{
#' p.mean <- c(0.5, 0.15, 0.35)
#' p.se   <- c(0.035, 0.025, 0.034)
#' dirichlet_params(p.mean, p.se)
#' # True values: 100, 30, 70
#' }
dirichlet_params <- function(p.mean, sigma){
  n.params <- length(p.mean)
  if(n.params != length(sigma)){
    stop("Length of mean different from length of sigma")
  }
  # Compute second moment
  p.2 <- sigma^2 + p.mean^2
  # Initialize alpa vector
  alpha <- numeric(n.params)
  for (i in 1:(n.params-1)){
    alpha[i] <- (p.mean[1] - p.2[1])*p.mean[i]/(p.2[1] - p.mean[1]^2)
  }
  alpha[n.params] <- (p.mean[1] - p.2[1])*(1-sum(p.mean[-n.params]))/(p.2[1] - p.mean[1]^2)
  return(alpha)
}