#' Probabilistic Sensitivity Analysis (PSA) dataset of Syndrome X
#'
#' Probabilistic sensitivity analysis (PSA) dataset from the cost-effectiveness analysis
#'  (CEA) example of Syndrome X from Jalal H and Alarid-Escudero F (2017) EVSI paper
#' @format A data frame with 10000 samples of the net monetary benefit (NMB) 
#' of three strategies and four input parameters: 
#' \describe{
#'  \item{nmbA}{NMB of Strategy A}
#'  \item{nmbB}{NMB of Strategy B}
#'  \item{nmbC}{NMB of Strategy C}
#'  \item{MeanVisitsA}{Mean number of visits with strategy A} 
#'  \item{MeanVisitsB}{Mean number of visits with strategy B} 
#'  \item{ProbFailingA}{Probability of failing treatment with strategy A} 
#'  \item{ProbFailingB}{Probability of failing treatment with strategy B} 
#' }
#' @references 
#' Jalal H, Alarid-Escudero F. A General Gaussian Approximation Approach 
#' for Value of Information Analysis. Under review. 2017. 
"syndX"