#' Probabilistic Sensitivity Analysis (PSA) dataset 
#'
#' Probabilistic sensitivity analysis (PSA) dataset from the cost-effectiveness 
#' analysis (CEA) example of Jalal et al. (2013) linear regression metamodeling paper
#' @format A data frame with 10000 samples of the effectiveness and costs of three strategies 
#'  and five input parameters: 
#' \describe{
#'  \item{Iteration}{PSA sample index}
#'  \item{Chemo_Cost}{Cost of chemotherapy strategy}
#'  \item{Chemo_Eff}{Effectiveness of chemotherapy strategy} 
#'  \item{Radio_Cost}{Cost of radiation strategy}
#'  \item{Radio_Eff}{Effectiveness of radiation strategy} 
#'  \item{Surg_Cost}{Cost of surgery strategy}
#'  \item{Surg_Eff}{Effectiveness of surgery strategy} 
#'  \item{pFailChemo}{Pobability of failing chemotherapy}
#'  \item{pFailRadio}{Probability of failing radiotherapy}
#'  \item{pFailSurg}{Probability of failing surgery}
#'  \item{pDieSurg}{Probability of dying because of surgery}
#'  \item{muDieCancer}{Cancer-specific mortality rate}
#'  \item{cChemo}{Chemotherapy cost ($)}
#'  \item{cRadio}{Radiotherapy cost ($)}
#'  \item{cSurg}{Surgery cost($)}
#' }
#' @references 1. Jalal H, Dowd B, Sainfort F, Kuntz KM. Linear regression 
#'  metamodeling as a tool to summarize and present simulation model results. 
#'  Med Decis Making. 2013;33(7):880–90. 
#'  \url{http://www.ncbi.nlm.nih.gov/pubmed/23811758}
"psa"