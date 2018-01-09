#' Expected Value of Perfect Information (EVPI)
#'
#' \code{evpi} is used to compute the expected value of perfect information 
#' (EVPI) from a probabilistic sensitivity analysis (PSA) dataset.
#' @param v.wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @param m.e Matrix of effectiveness. Each column corresponds to a vector of
#' effectiveness.
#' @param m.c Matrix of costs. Each column corresponds to a vector of
#' costs.
#' @param pop A scalar that corresponds to the total population
#' @keywords expected value of perfect information; net monetary benefit
#' @section Details:
#' \code{evpi} calculates the value of eliminating all the uncertainty of a 
#' cost-effectiveness analysis at each WTP threshold.
#' @return evpi A data frame with the EVPI at each WTP threshold. 
#'
evpi <- function(v.wtp, m.e, m.c, pop = 1){
  # Load required packages
  require(matrixStats)
  if(!(ncol(m.e) == ncol(m.c))){
    stop("Matrices of effectiveness and costs do not have same number of strategies.")
  }
  if(ncol(m.e)<2){
    stop("You need at least two different strategies to compute EVPI.")
  }
  # Create scalar with number of simulations
  n.sim <- nrow(m.e)
  # Create scalar with number of strategies (i.e. number of columns of 
  # effectiveness matrix)
  n.str <- ncol(m.e)
  # Data frame to store EVPI for each WTP threshold
  df.evpi <- as.data.frame(array(0, dim = c(length(v.wtp), 2)))
  # Name data frame's columns
  colnames(df.evpi) <- c("WTP", "EVPI")
  # Assign vector with WTP thresholds to first column of `evpi`
  df.evpi$WTP <- v.wtp
  # Estimate the Loss matrix and EVPI at each WTP threshold
  for(l in 1:length(v.wtp)){
    # Compute NMB with vector indexing
    nmb <-  v.wtp[l]*m.e - m.c
    ## Find the optimal strategy with current info
    d.star <- which.max(colMeans(nmb))
    ## Calculate the opportunity loss from choosing d.star for each strategy
    loss <- nmb - nmb[, d.star]
    ## Compute EVPI
    df.evpi$EVPI[l] <- mean(rowMaxs(as.matrix(loss))) * pop# needs to be a numeric matrix
  }
  #Return a data frame
  class(df.evpi) <- "evpi"
  return(df.evpi)
}