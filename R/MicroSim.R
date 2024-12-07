#' A Microsimulation Function
#'
#' Computes a microsimulation matrix keeping track of what happens to each inidividual at each cycle
#' @param m.Transitions Transition probability matrix or array 
#' @param p0 Initial state vector
#' @param n.Ind Number of hypothetical subjects simulated in the model 
#' @param n.Cycles Total number of cycles to run the model
#' @param seed Seed number for random number generator (optional), default is NULL
#' @keywords Microsimulation
#' @return m.Microsim A (n.Ind x n.Cycles) matrix showing what happens to each indiviaul at each cycle
#' MicroSim()
#'
#ADD the code 