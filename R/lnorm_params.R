#' Calculate location and scale parameters of a log-normal distribution.
#' 
#' Function to calculate the location, \eqn{\mu}, and scale, \eqn{\sigma}, 
#' parameteres of a log-normal distribution based on the method of moments (MoM) 
#' using the mean \eqn{m} and variance \eqn{v} of the non-logarithmized random 
#' variable of interest.
#' @param m Scalar with the mean of the random variable.
#' @param v Scalar with the variance of the random variable.
#' (i.e., squared standar error).
#' @keywords log-normal distribution; method of moments
#' @section Details:
#' Based on method of moments. If \eqn{m} is the mean and 
#' \eqn{v} is the variance of the random variable, then the
#' the location, \eqn{\mu}, and scale, \eqn{\sigma}, parameteres are computed 
#' as follows
#' \deqn{\mu = \ln{(\frac{m}{\sqrt{(1 + \frac{v}{m^2})}})}}
#' and
#' \deqn{\sigma = \sqrt{\ln{( 1 + \frac{v}{m^2})}}}
#' 
#' @references
#' \enumerate{
#' \item Ginos BF. Parameter Estimation for the Lognormal Distribution. 
#' Brigham Young University; 2009.
#' \item Log-normal distribution. (2017, April 20). In Wikipedia, The Free 
#' Encyclopedia. Retrieved 16:47, April 23, 2017, 
#' from https://en.wikipedia.org/w/index.php?title=Log-normal_distribution&oldid=776357974
#' }
#' @return 
#' mu Location parameter of log-normal distribution
#' sigma Scale parameter of log-normal distribution
#' @examples
#' \dontrun{
#' m <- 3
#' v <- 0.01
#' lnorm_params(m, v)
#' # True values: 100, 30, 70
#' }
lnorm_params <- function(m = 1, v = 1){
  ### Sanity checkd
  if(m <= 0){stop("'m' needs to be greater than 0")}
  if(v <= 0){stop("'v' needs to be greater than 0")}
  mu    <- log(m/sqrt(1 + v/m^2))
  sigma <- sqrt(log(1 + v/m^2))
  return(list(mu = mu,
              sigma = sigma))
}