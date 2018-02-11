#' Calculate shape and scale (or rate) parameters of a gamma distribution.
#' 
#' Function to calculate the shape, \eqn{\alpha}, and scale, \eqn{\theta}, (or rate, \eqn{\beta}) 
#' parameteres of a gamma distribution based on the method of moments (MoM) 
#' using the mean \eqn{\mu} and standard deviation \eqn{\sigma} of the random 
#' variable of interest.
#' @param mu Scalar with the mean of the random variable.
#' @param sigma Scalar with the standard deviation of the random variable.
#' @param scale Logical variable indicating scale parameterization of gamma distribution
#' (Default is TRUE). If FALSE, rate parameterization is retrieved
#' @keywords gamma distribution; method of moments
#' @section Details:
#' Based on method of moments. If \eqn{\mu} is the mean and 
#' \eqn{\sigma} is the standard deviation of the random variable, then the
#' the shape, \eqn{\alpha}, scale, \eqn{\theta}, and rate, \eqn{\beta}, parameteres are computed 
#' as follows
#' \deqn{\alpha=\frac{\mu^2}{\sigma^2},}
#' \deqn{\theta = \frac{\sigma^2}{\mu}}
#' and
#' \deqn{\beta = \frac{\mu}{\sigma^2}}
#' 
#' @references
#' \itemize{
#' \item Gamma distribution. (2018, February 7). In Wikipedia, The Free 
#' Encyclopedia. Retrieved 17:23, February 11, 2018, 
#' from https://en.wikipedia.org/w/index.php?title=Gamma_distribution&oldid=824541785
#' }
#' @return 
#' shape Shape parameter of gamma distribution
#' scale Scale parameter of gamma distribution (If scale=TRUE)
#' rate Rate parameter of gamma distribution (If scale=FALSE)
#' @examples
#' \dontrun{
#' mu    <- 2
#' sigma <- 1
#' # Scale specification 
#' gamma_params(mu, sigma)
#' # Rate specification 
#' gamma_params(mu, sigma, scale = FALSE)
#' }
gamma_params <- function(mu, sigma, scale = TRUE){
  if (scale){
    shape <- (mu^2)/(sigma^2)
    scale <- (sigma^2)/mu
    params <- list(shape = shape, 
                   scale = scale)
  } else {
    shape <- (mu^2)/(sigma^2)
    rate  <- mu/(sigma^2)
    params <- list(shape = shape, 
                   rate  = rate)
  }
  return(params)
}