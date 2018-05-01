#' Estimation of the Expected Value of Partial Perfect Information (EVPPI)
#' using a linear regression metamodel approach
#'
#' \code{evppi_lrmm} is used to estimate the Expected Value of Partial Perfect
#' Information (EVPPI) using a linear regression metamodel approach from a
#' probabilistic sensitivity analysis (PSA) dataset.
#' @param nmb Matrix of net monetary benefits (NMB). Each column corresponds to
#' the NMB of a different strategy.
#' @param params Vector or matrix of parameters.
#' @param sel.params A vector including the column index of parameters for
#' which EVPPI should be estimated.
#' @param sel.gam Logical variable indicating if a generalized additive model,
#' GAM, (i.e., a spline model) should be fitted (Default = T). If FALSE,
#' a polynomial of degree k is fitted.
#' @param k Scalar with the order of basis functions for the spline model
#' if (sel.gam == T) or the degree of polynomial if (sel.gam != T)
#' @param verbose Logical variable indicating if estimation progress should be
#' reported.
#' @keywords Expected Value of Partial Perfect Information
#' @keywords Linear regression metamodel
#' @keywords Splines
#' @details
#' The expected value of partial pefect information (EVPPI) is the expected
#' value of perfect information from a subset of parameters of interest,
#' \eqn{\theta_I} of a cost-effectiveness analysis (CEA) of \eqn{D} different
#' strategies with parameters \eqn{\theta = \{ \theta_I, \theta_C\}}, where
#' \eqn{\theta_C} is the set of complimenatry parameters of the CEA. The
#' function \code{evppi_lrmm} computes the EVPPI of \eqn{\theta_I} from a
#' matrix of net monetary benefits \eqn{B} of the CEA. Each column of \eqn{B}
#' corresponds to the net benefit \eqn{B_d} of strategy \eqn{d}. The function
#' \code{evppi_lrmm} computes the EVPPI using a linear regression metamodel
#' approach following these steps:
#' \enumerate{
#' \item Determine the optimal strategy \eqn{d^*} from the expected net
#' benefits \eqn{\bar{B}}
#' \deqn{d^* = argmax_{d} \{\bar{B}\}}
#' \item Compute the opportunity loss for each \eqn{d} strategy, \eqn{L_d}
#' \deqn{L_d = B_d - B_{d^*}}
#' \item Estimate a linear metamodel for the opportunity loss of each \eqn{d}
#' strategy, \eqn{L_d}, by regressing them on the spline basis functions of
#' \eqn{\theta_I}, \eqn{f(\theta_I)}
#' \deqn{L_d = \beta_0 + f(\theta_I) + \epsilon,}
#' where \eqn{\epsilon} is the residual term that captures the complementary
#' parameters \eqn{\theta_C} and the difference between the original simulation
#' model and the metamodel.
#' \item Compute the EVPPI of \eqn{\theta_I} using the estimated losses for
#' each \eqn{d} strategy, \eqn{\hat{L}_d} from the linear regression metamodel
#' and applying the following equation:
#' \deqn{EVPPI_{\theta_I} = \frac{1}{K}\sum_{i=1}^{K}\max_d(\hat{L}_d)}
#' The spline model in step 3 is fitted using the `mgcv` package.
#' }
#' @return evppi A numeric vector of size one with the EVPPI of the selected
#' parameters
#' @references
#' \enumerate{
#' \item Jalal H, Alarid-Escudero F. A General Gaussian Approximation Approach
#' for Value of Information Analysis. Med Decis Making. 2018;38(2):174-188.
#' \item Strong M, Oakley JE, Brennan A. Estimating Multiparameter Partial
#' Expected Value of Perfect Information from a Probabilistic Sensitivity
#' Analysis Sample: A Nonparametric Regression Approach. Med Decis Making.
#' 2014;34(3):311–26.
#' }
#' @examples
#' ## Load mgcv package and matrixStats
#' library(mgcv)
#' library(matrixStats)
#' ## Load PSA dataset
#' data(syndX)
#' ## Net monetary benefit (NMB) matrix
#' nmb <- syndX[, 5:7]
#' ## Matrix of model parameter inputs values theta
#' theta <- syndX[, 1:4]
#' ## Optimal strategy (d*) based on the highest expected NMB
#' d.star <- which.max(colMeans(nmb))
#' d.star
#' ## Define the Loss matrix
#' loss <- nmb - nmb[, d.star]
#' ## Estimate EVPPI for parameter 1 (MeanVisitsA)
#' evppi_lrmm(nmb = nmb, params = theta, sel.params = 1, verbose = TRUE)
evppi_lrmm <- function (nmb = NULL, params = NULL, sel.params = 1,
                            sel.gam = T, k = NULL,
                            verbose = F)
{
  library(mgcv, matrixStats)
  if (is.null(nmb)) {
    stop("A matrix of NMB, 'nmb', hasn't been specified")
  }
  if (is.null(params)) {
    stop("A matrix of parameters, 'params', hasn't been specified")
  }
  if (is.null(dim(nmb))) {
    stop("'nmb' must be an array with at least two strategies")
  }
  n.sel.params <- length(sel.params)
  if (is.null(dim(params))) {
    n.params <- 1
    if (sel.params > 1) {
      stop("Parameter selected is not included in the vector of parameters, 'params'")
    }
  }
  else {
    n.params <- ncol(params)
  }
  if (n.sel.params > n.params) {
    stop("Number of selected parameters exceeds the number of parameters on 'params' (the matrix or vector of parameters)")
  }

  ### Check for correct input k
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  if(!is.null(k)){
    if(!is.wholenumber(k)){
      stop("Parameter 'k' should be either NULL or an integer")
    }
  }

  if (verbose) {
    print(paste("Estimating EVPPI of", n.sel.params, "parameters"))
  }
  n.sim <- nrow(nmb)
  n.strategies <- ncol(nmb)
  d.star <- which.max(colMeans(nmb))
  d.star
  Loss <- nmb - nmb[, d.star]
  lrm <- vector("list", n.strategies)
  Lhatp <- matrix(0, nrow = n.sim, ncol = n.strategies)
  if(sel.gam == T){
    if (is.null(k)){
      for (d in 1:n.strategies) {
        if(d == d.star){
          if (verbose) {
            print(paste("Strategy", d, "is d*; loss of d* = 0"))
          }
          Lhatp[, d] <- 0
        } else {
          if (verbose) {
            print(paste("Constructing metamodel for the Loss of strategy",
                        d))
          }
          if (length(sel.params) == 1) {
            if (n.params == 1) {
              lrm[[d]] <- gam(Loss[, d] ~ s(params))
            }
            else {
              lrm[[d]] <- gam(as.formula(paste("Loss[, d] ~ s(",
                                               colnames(params)[sel.params], ")")), data = params)
            }
          }
          else {
            lrm[[d]] <- gam(as.formula(paste("Loss[, d] ~ s(",
                                             paste(colnames(params[, sel.params]), collapse = ") + s("),
                                             ") + ti(", paste(colnames(params[, sel.params]),
                                                              collapse = ", "), ")")), data = params)
          }
          Lhatp[, d] <- lrm[[d]]$fitted
        }
      }
    } else{
      print(paste0("Spline with k = ", k, " basis functions selected"))
      for (d in 1:n.strategies) {
        if(d == d.star){
          if (verbose) {
            print(paste("Strategy", d, "is d*; loss of d* = 0"))
          }
          Lhatp[, d] <- 0
        } else {
          if (verbose) {
            print(paste("Constructing metamodel for the Loss of strategy",
                        d))
          }
          if (length(sel.params) == 1) {
            if (n.params == 1) {
              lrm[[d]] <- gam(Loss[, d] ~ s(params, k = k))
            }
            else {
              lrm[[d]] <- gam(as.formula(paste("Loss[, d] ~ s(",
                                               colnames(params)[sel.params], ", k = ", k,")")), data = params)
            }
          }
          else {
            lrm[[d]] <- gam(as.formula(paste("Loss[, d] ~ s(",
                                             paste(colnames(params[, sel.params]), collapse = paste0(", k = ", k,") + s(")),
                                             ",k=", k, ") + ti(", paste(colnames(params[, sel.params]),
                                                                        collapse = ", "), ", k=",3,")")), data = params)
          }
          Lhatp[, d] <- lrm[[d]]$fitted
        }
      }
    }
  } else { ## sel.gam != T
    if (is.null(k)){
      k <- 1
    }
    print(paste0("Polynomial of degree k = ", k, " selected"))
    for (d in 1:n.strategies) { # d <- 2
      if(d == d.star){
        if (verbose) {
          print(paste("Strategy", d, "is d*; loss of d* = 0"))
        }
        Lhatp[, d] <- 0
      } else {
        if (verbose) {
          print(paste("Constructing metamodel for the Loss of strategy",
                      d))
        }
        if (length(sel.params) == 1) {
          if (n.params == 1) {
            lrm[[d]] <- lm(Loss[, d] ~ polym(params, degree = k))
          }
          else {
            lrm[[d]] <- lm(as.formula(paste("Loss[, d] ~ polym(",
                                            paste(colnames(params)[sel.params], collapse = ","),
                                            ", degree = ", k, ", raw = TRUE)")), data = params)
          }
        }
        else {
          lrm[[d]] <- lm(as.formula(paste("Loss[, d] ~ polym(",
                                          paste(colnames(params[, sel.params]), collapse = ","),
                                          ", degree = ", k, ", raw = TRUE)")), data = params)
        }
        Lhatp[, d] <- lrm[[d]]$fitted
      }
    }
  }
  evppi <- mean(rowMaxs(Lhatp))
  return(c(evppi = round(evppi, 1)))
}
