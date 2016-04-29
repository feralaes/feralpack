#' Multinomial Logistic (MNL) Metamodeling for Decision Sensitivity Analysis (DSA)
#'
#' \code{mnl} is used to compute the probability of each startegy being optimal
#' over different values of a parametere of interest from a probabilistic 
#' sensitivity analysis (PSA) dataset.
#' @param strategies String vector with the name of the strategies
#' @param x Matrix with the model inputs or parameters
#' @param y Matrix with the model outputs
#' @param parm String with the name of the parameter of interest
#' @param range Range of the parameter of interest. Default: NULL range. If
#' range=NULL, the 2.5th and 9.75th percentile of the parameter
#' of interest will be used as lower and upper bounds of the 
#' range, respectively.
#' @param txtsize Font size for ggplot graph. Default: 12
#' @keywords decision sensitivity analysis; multinomial logistic metamodel
#' @section Details:
#' \code{mnl} computes the probability of each of the strategies being 
#' cost-effective at over different values of the parameter of interest.
#' @return mnl A melted data frame with each strategy's probability of being 
#' cost-effective over a range of values of the parameter of interest.
#'
mnl <- function(x, y, parm,
                strategies = NULL, 
                range = NULL,
                txtsize = 12){
  # Load required packages
  require(reshape2)
  require(matrixStats)
  require(VGAM)
  # Extract parameter column number in x matrix
  par.col <- which(colnames(x)==parm)
  # Create scalar with number of strategies (i.e. number of columns of `y`)
  n.strategies <- ncol(y)
  #Determine range of of the parameer to be plotted
  if (is.null(range)){ #If user does not define a range
    #Default range given by the domain of the parameter's sample
    #vector to define 400 samples between the 2.5th and 97.5th percentiles
    percentiles = seq(2.5, 97.5, length = 400) 
    j = round(percentiles*(length(x[,par.col])/100)) #indexing vector;j=round(y*n/100) where n is the size of vector of interest
    vector<-sort(x[j, par.col]) 
  }
  else{ #If user defines a range  
    vector <- seq(range[1],range[2],length.out=400)
  }
  # If the name of the strategies is not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)){
    strategies <- paste(rep("Strategy_", n.strategies), seq(1, n.strategies), sep = "")
  }
  ## Vector with optimal strategy at all PSA samples
  d.star <- data.frame(max.col(y))
  colnames(d.star) <- "Optimal"
  # Create data frame to estimate MNL metamodel
  mnl.data <- data.frame(d.star, x)
  # Estimate MNL metamodel
  mnl <- vglm(Optimal ~ ., data = mnl.data, family = multinomial)
  # Generate matrix with parameter means for prediction 
  mnl.fit <- as.data.frame(matrix(rep(colMeans(x)),
                           nrow = length(vector), 
                           ncol = ncol(x), 
                           byrow = T))
  colnames(mnl.fit) <- colnames(x)
  # Substitute parameter of interest with values of interest
  mnl.fit[, parm] <- vector
  # Compute predicted probabilities
  mnl.pred <- data.frame(vector,
                         predict(mnl, newdata = mnl.fit, type = "response"))
  colnames(mnl.pred) <- c(parm, strategies)
  mnl.pred <- melt(mnl.pred, id.vars = parm, 
                   value.name = "Probability", 
                   variable.name = "Strategy")
  mnl.pred$parm <- mnl.pred[, parm]
  mnl.gg <- ggplot(data = mnl.pred, aes(x = parm, y = Probability, 
                                        color = Strategy,
                                        shape = Strategy)) +
    geom_point(size = 2) + #maybe shape=3;18;21;124; Other shapes: http://sape.inf.usi.ch/quick-reference/ggplot2/shape
    geom_line() +
    ggtitle("Decision Sensitivity Analysis") + 
    xlab(parm) +
    ylab("Probability of Strategy Being Optimal") +
    scale_colour_hue("Strategy", l=50) +
    scale_x_continuous(breaks=number_ticks(6)) + #Adjust for number of ticks in x axis
    scale_y_continuous(breaks=number_ticks(6)) +
    theme_bw() +
    theme(legend.position="bottom",legend.title=element_text(size = txtsize),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=txtsize+2),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize))
  return(list(mnl = mnl.pred,
              mnl.gg = mnl.gg))
}