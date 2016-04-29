#' Expected Value of Perfect Information (EVPI)
#'
#' \code{evpi} is used to compute the expected value of perfect information 
#' (EVPI) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @param Outcomes Matrix with the model outputs. The outcomes must be ordered 
#' in such a way that for each strategy the cost must appear first then the 
#' effectiveness.
#' @keywords expected value of perfect information; net monetary benefit
#' @section Details:
#' \code{evpi} calculates the value of eliminating all the uncertainty of a 
#' cost-effectiveness analysis at each \code{wtp} threshold.
#' @return evpi A data frame with the EVPI at each WTP threshold. 
#'
evpi <- function(wtp, Outcomes){
  # Load required packages
  require(reshape2)
  require(matrixStats)
  # Create scalar with number of simulations
  n.sim <- nrow(Outcomes)
  # Create scalar with number of strategies (i.e. number of columns of 
  # `Outcomes` divided by two)
  n.strategies <- ncol(Outcomes)/2
  # Data frame to store EVPI for each WTP threshold
  evpi <- as.data.frame(array(0, dim = c(length(wtp), 2)))
  # Name data frame's columns
  colnames(evpi) <- c("WTP", "EVPI")
  # Assign vector with WTP thresholds to first column of `evpi`
  evpi$WTP <- wtp
  # Vector to index costs
  costInd <- seq(1, 2*n.strategies, by = 2)
  # Vector to index effectiveness
  effInd  <- seq(2, 2*n.strategies, by = 2)
  # Estimate the Loss matrix and EVPI at each WTP threshold
  for(l in 1:length(wtp)){
    # Compute NMB with vector indexing
    nmb <-  wtp[l]*Outcomes[, effInd]-Outcomes[, costInd] 
    ## Find the optimal strategy with current info
    d.star <- which.max(colMeans(nmb))
    ## Calculate the opportunity loss from choosing d.star for each strategy
    loss <- nmb - nmb[, d.star]
    ## Compute EVPI
    evpi$EVPI[l] <- mean(rowMaxs(as.matrix(loss))) # needs to be a numeric matrix
  }
  #Return a data frame
  class(evpi) <- "evpi"
  return(evpi)
}