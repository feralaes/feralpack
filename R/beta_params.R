#' Calculate alpha and beta parameters of Beta distribution.
#' 
#' Function to calculate the alpha and beta parameters of the Beta distribution
#' based on the method of moments using the mean \eqn{\mu} and standard 
#' deviation \eqn{\sigma} of the random variable of interest.
#' @param mean Mean of the random variable.
#' @param sigma Standard deviation of the random variable (i.e., standar error).
#' @keywords beta distribution; methods of moments
#' @section Details:
#' Based on methods of moments. If \eqn{\mu} is the mean and 
#' \eqn{\sigma} is the standard deviation of the random variable, then
#' \deqn{\alpha = (\frac{1-\mu}{\sigma^2} - \frac{1}{\mu}) \mu^2}
#' and
#' \deqn{\beta = \alpha (\frac{1}{\mu} -1)}
#' 
#' @return alpha Alpha parameter of beta distribution
#' @return beta Beta parameter of beta distribution
beta_params <- function(mean, sigma){
  alpha <- ((1-mean) / sigma^2 - 1 / mean) * mean^2 
  beta  <- alpha*(1 / mean - 1)
  params <- list(alpha = alpha, beta = beta)
  return(params)
}