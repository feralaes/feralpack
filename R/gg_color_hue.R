#' \code{ggplot2}-like colour scale in HCL space
#'
#' Function for number of ticks in axis of \code{ggplot2} plots.
#' @param n Number of colours to return
#' @param hue_min Minimum hue value in the range [0, 360].
#' @param hue_max Maximum hue value in the range [0, 360].
#' @param l Luminance in the range [0, 100].
#' @param c Chroma of the colour.
#' @keywords ggplot2
#' @section Details:
#' @examples 
#' gg_color_hue(10)
gg_color_hue <- function(n, hue_min = 15, hue_max = 375, l = 65, c = 100) {
  hues = seq(hue_min, hue_max, length = n + 1)
  hcl(h = hues, l = l, c = c)[1:n]
}