#' Plot of Expected Value of Perfect Information (EVPI)
#'
#' Plots the EVPI as a \code{ggplot2} object calculated with \code{\link{evpi}}.
#' @param evpi Object of class \code{evpi}. A data frame produced by function 
#'  \code{evpi} with the EVPI for each willingness-to-pay (WTP) threshold
#' @param title String with graph's title
#' @param txtsize number with text size
#' @param currency String with currency used in the cost-effectiveness analysis (CEA). 
#'  Default: $, but it could be any currency symbol or word (e.g., £, €, peso)
#' @keywords expected value of perfect information
#' @return plot.gg A \code{ggplot2} object with the EVPI
#'
plot.evpi <- function(evpi, 
                      title = "Expected Value of Perfect Information", 
                      txtsize = 12,
                      currency = "$"){
  # Create EVPI data frame from EVPI S3 object
  evpi <- data.frame(WTP = evpi$WTP, EVPI = evpi$EVPI)
  # Load required packages
  require(ggplot2) 
  require(scales) 
  ggplot(data = evpi, aes(x = WTP/1000, y = EVPI)) +
    #geom_point() +
    geom_line() +  
    ggtitle(title) + 
    scale_x_continuous(labels = comma, breaks = number_ticks(20))+ 
    scale_y_continuous(labels = comma, breaks = number_ticks(6))+
    xlab(paste("Willingness to Pay (Thousand ", currency, "/QALY)", sep = "")) +
    ylab(paste("EVPI (", currency, ")", sep = "")) +
    theme_bw() +
    theme(legend.position="bottom",legend.title=element_text(size = txtsize),
          legend.key = element_rect(colour = "black"),
          legend.text = element_text(size = txtsize),
          title = element_text(face="bold", size=txtsize+2),
          axis.title.x = element_text(face="bold", size=txtsize),
          axis.title.y = element_text(face="bold", size=txtsize),
          axis.text.y = element_text(size=txtsize),
          axis.text.x = element_text(size=txtsize))
}