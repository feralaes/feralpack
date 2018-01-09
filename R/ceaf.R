#' Cost-Effectiveness Acceptability Curves (CEAC) and Frontier (CEAF)
#'
#' \code{ceaf} is used to compute and plot the cost-effectiveness acceptability 
#' curves (CEAC) and frontier (CEAF) from a probabilistic sensitivity 
#' analysis (PSA) dataset.
#' @param v.wtp Numeric vector with willingness-to-pay (WTP) thresholds
#' @param strategies String vector with the name of the strategies
#' @param m.e Matrix of effectiveness. Each column corresponds to a vector of
#' effectiveness.
#' @param m.c Matrix of costs. Each column corresponds to a vector of
#' costs.
#' @param currency Currency used in ceaf plot.
#' @param ceaf.out Logical variable indicating if ceaf data frame should 
#' be returned.
#' @keywords cost-effectiveness acceptability curves
#' @keywords cost-effectiveness acceptability frontier
#' @section Details:
#' \code{ceaf} computes the probability of each of the strategies being 
#' cost-effective at each \code{v.wtp} threshold.
#' @return df.ceaf A melted data frame with each strategy's probability of being 
#' cost-effective at each WTP threshold.
#' @return gg.ceaf A ggplot object with each strategy's probability of being 
#' cost-effective at each WTP threshold.
#' @examples 
#' # Load PSA dataset
#' data(psa)
#' # Name of strategies
#' strategies <- c("Chemo", "Radio", "Surgery")
#' # Vector of WTP thresholds
#' v.wtp <- seq(1000, 150000, by = 10000)
#' # Matrix of costs
#' m.c <- psa[, c(2, 4, 6)]
#' # Matrix of effectiveness
#' m.e <- psa[, c(3, 5, 7)]
#' out <- ceaf(v.wtp = v.wtp, strategies = strategies, 
#'             m.e = m.e , m.c = m.c,
#'             ceaf.out = TRUE)
ceaf <- function(v.wtp, strategies = NULL, m.e, m.c, currency = "$", ceaf.out = FALSE){
  # Load required packages
  library(reshape2)
  library(ggplot2)
  library(scales)
  if(!ncol(m.e) == ncol(m.c)){
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
  # Matrix to store indicator of CEAC
  m.cea  <- array(0, dim = c(length(v.wtp), n.str))
  # Vector to store indicator of strategy at CEAF
  v.ceaf <- array(0, dim = c(length(v.wtp), 1))
  
  # If the name of the strategies is not provided, generate a generic vector
  # with strategy names
  if (is.null(strategies)){
    strategies <- paste(rep("Strategy_", n.strategies), seq(1, n.strategies), sep = "")
  }
  for(l in 1:length(v.wtp)){
    m.nmb <-  v.wtp[l]*m.e - m.c # Effectiveness minus Costs
    # Calculate point of CEAF, i.e., the strategy with the highest expected NMB
    v.ceaf[l, 1] <- which.max(colMeans(m.nmb))
    # Calculate points in CEAC, i.e, the probability that each strategy is cost-effective
    max.nmb <- max.col(m.nmb)
    opt <- table(max.nmb)
    m.cea[l, as.numeric(names(opt))] <- opt/n.sim
  }
  m.ceaf <- m.cea[cbind(1:length(v.wtp), v.ceaf)]
  
  df.cea <- data.frame(cbind(v.wtp, m.cea, m.ceaf))
  colnames(df.cea) <- c("WTP", strategies, "Frontier")
  
  df.ceac <- melt(df.cea, 
                  id.vars = "WTP", 
                  variable.name = "Strategy")
  
  ## Plot CEAC & CEAF
  # Format to plot frontier
  strats <- 1:(length(unique(df.ceac$Strategy))-1)
  point.shapes <- c(strats+14, 0) # Shapes: http://sape.inf.usi.ch/quick-reference/ggplot2/shape
  colors <- c(gg_color_hue(length(strats)), "#696969")
  point.size <- c(rep(2, length(strats)), 4) # Trick consists on firts define size as aes then manually change it
  # Plot CEAC & CEAF
    gg.ceaf <- ggplot(data = df.ceac, aes(x = WTP/1000, y = value)) +
      geom_point(aes(shape = Strategy, color = Strategy, size = Strategy)) +
      geom_line(aes(color = Strategy)) +
      ggtitle("Cost-Effectiveness Acceptability Curves and Frontier") + 
      # scale_colour_hue(l=50, values=colors) +
      scale_x_continuous(breaks=number_ticks(20))+
      scale_shape_manual(values=point.shapes) +
      #scale_shape(solid=TRUE) +
      scale_color_manual(values=colors) + 
      scale_size_manual(values = point.size) +
      #scale_alpha_manual(values=c(rep(0, length(strats)), 0.5)) + 
      xlab(paste("Willingness to pay (Thousand ", currency,"/QALY)", sep = "")) +
      ylab("Pr Cost-Effective") +
      theme_bw(base_size = 14) +
      theme(legend.position = "bottom")
  # Return a data frame of class ceaf
  class(df.ceac) <- "ceaf"
  if(ceaf.out){
    print(gg.ceaf)
  }
  out <- list(df.ceac = df.ceac,
              gg.ceaf = gg.ceaf)
  return(out)
}