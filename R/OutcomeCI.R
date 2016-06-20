#' Outcome uncertainty
#'
#' This function created 95% confidence intervals around the outcome values
#' @param outcomes Vector with the different rewards
#' @keywords Microsimulation uncertainty outcomes
#' @return table with the mean outcome values and the upper and lower value of the 95% confidence interval
#' CEA()
#'


OutcomeCI <- function(outcomes){
  # Calculate standard deviation of the mean outcomes
  m.sd_outcomes <- matrix(0, nrow = 1, ncol = ncol(outcomes$ind))
  for (l in 1:ncol(outcomes$ind)) {
    m.sd_outcomes[, l] <- sd(outcomes$ind[, l])
  }
  # Calculate the 95%-CI of the mean outcomes
  max <- colMeans(outcomes$ind) + (1.96 * (m.sd_outcomes / sqrt(nrow(outcomes$ind))))
  min <- colMeans(outcomes$ind) - (1.96 * (m.sd_outcomes / sqrt(nrow(outcomes$ind))))
  # Combine as a data frame
  df.outcome <- cbind((colMeans(outcomes$ind)), t(max), t(min))
  colnames(df.outcome) <- c("mean", "upper", "lower")

  return(df.outcome)
}
