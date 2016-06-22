#' A cost-effectiveness analysis function
#'
#' This function performs a cost-effectivenss analysis based on cost and reward data from the simulation.
#' @param outcomes Vector with the different rewards
#' @keywords Microsimulation cost-effectiveness
#' @return table Cost effectivess ananlysis results including the ICER.
#' CEA()
#'

CEA <- function(outcomes){

  C  <- c(outcomes$mean[3], outcomes$mean[5])
  E  <- c(outcomes$mean[2], outcomes$mean[4])

  # Calculate incremental costs and effects and ICERs
  DC <- C[2] - C[1]
  names(DC) <- "Incremental costs"
  DE <- E[2] - E[1]
  names(DE) <- "QALYs gained"
  ICER <- DC / DE
  names(ICER) <- "ICER"
  results <- c(DC, DE, ICER)

  # Create full incremental cost-effectiveness analysis table
  costs   <- round(C, 2)
  effects <- round(E, 2)

  strategies <- c("Control", "Treatment")
  DC   <- c("", as.character(round(DC, 2)))
  DE   <- c("", as.character(round(DE, 2)))
  ICER <- c("", as.character(round(ICER, 2)))

  table_cohort <- cbind(strategies, costs, effects, DC, DE, ICER)
  table_cohort <- as.data.frame(table_cohort)
  table_cohort
}
