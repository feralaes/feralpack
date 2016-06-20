#' A bas
#'
#' This function computes the microsimulation trace and transition array for a given matrix with individual history
#' @param r Rate
#' @param t Time
#' @keywords rate probability
#' @return Probability for the time interval similar to the rate
#' RateToProb()
#'

RateToProb = function(r, t) {
  1 - exp(- r * t)
}
