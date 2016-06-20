#' A probability function
#'
#' This function converts a probability into a odds
#' @param p The probability
#' @keywords probability odds
#' @return The odds for the given probability.
#'
#' ProbToOdds()
#'

# Define a Prob to Odds function
ProbToOdds = function(p) {
  odds = p /(1 - p)
  list(odds)}

#' @examples
#'p <- 0.25
#'odds <- ProbToOdds(p)
#'print(odds)
