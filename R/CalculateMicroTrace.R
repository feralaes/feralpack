#' A Microsimulation Function
#'
#' This function computes the microsimulation trace and transition array for a given matrix subject patient history
#' @param m.Microsim A matrix showing what happens to each subject at each cycle
#' @param state_names Names of the health states 
#' @param p0 Initial state vector
#' @keywords Microsimulation
#' @return trace A (n.Cycles x n.States) matrix with microsimulation trace
#' @return trans A (n.States x n.States x n.Cycles) array with transitions over time
#' @return trans_ind A (n.States x n.States x n.Cycles x n.Ind) array with transitions over time per subject
#' @return trans_cycle A(n.States x n.States x n.Ind) array showing all the transitions during all time points for a subject
#' CalculateMicroTrace()
#'
#ADD the code 