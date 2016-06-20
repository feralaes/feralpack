#' CalculateMicroTrace
#'
#' This function computes the microsimulation trace and transition array for a given matrix with individual history
#' @param m.Microsim A microsimulation matrix showing what happens to each individual at each cycle
#' @param state_names Names of the health states
#' @param p.0 Initial state vector
#' @keywords Microsimulation
#' @return trace A(n.t x n.s) matrix with microsimulation trace
#' @return trans A (n.s x n.s x n.t) array with transitions over time
#' @return trans_ind A (n.s x n.s x n.t x n.i) array with transitions over time per individual
#' @return trans_sum_ind A (n.s x n.s x n.i) showing all the transitions during all time points for an individual
#' CalculateMircoTrace()
#'

CalculateMicroTrace <- function (m.Microsim, state_names, p.0) {

  ### Create a trace from the individual trajectories
  trace <- t(apply(m.Microsim, 2,
                   function(x) table(factor(x, levels = states, ordered = TRUE))))
  trace <-  trace / n.i   # Creat a distribution trace
  # Name the rows and columns of the matrix
  colnames(trace) <- state_names
  rownames(trace) <- cycle_names

  ### Create a transition matrix per individual
  # The results of the MicroSim function are used for that
  n.t    <- ncol(m.Microsim)
  n.i    <- nrow(m.Microsim)
  n.s    <- length(state_names)

  # Initialize trans array with four dimensions
  trans_ind <- array(0,
                     dim = c(n.s, n.s, n.t, n.i),
                     dimnames = list(state_names,  state_names, cycle_names, ind_names))

  # First row of first stach of trans array is equal to the initial state vector
  trans_ind[1, , 1, ] <- p.0

  # Create a loop for all individuals
  for (i in 1:n.i) {
    # Starting at column two until the end of n.t
    for(t in 2:n.t) {
      # Write an one at the right place in the transition matrix
      # The health state values function as co-ordinates
      # e.g. individual 1: 1, 1, 2, 1, 3, 4
      # This meant that at cycle two a 1 is added in the matrix at place (1, 2)
      trans_ind[m.Microsim [i, t - 1], m.Microsim [i, t], t, i] = 1
    }
  }

  # Create a summary transition array of all individuals
  trans <-  array(0,
                  dim = c(n.s, n.s, n.t),
                  dimnames = list(state_names,  state_names, cycle_names))

  # Sum the data for all cycles
  trans_sum_ind <-  array(0,
                          dim = c(n.s, n.s, n.i),
                          dimnames = list(state_names,  state_names, ind_names))
  for (s in seq(n.s)) {
    for (r in seq(n.s)) {
      for (t in seq(n.t)) {
        trans_ind[s, r, t, ] <- trans_ind[s, r, t, ]
        trans[s, r, t] <- sum(trans_ind[s, r, t, ])
      }
      for (i in seq(n.i)) {
        trans_sum_ind[s, r, i] <- sum(trans_ind[s, r, , i])
      }
    }
  }

  # Save all the results in a list.
  dataList <- list(trace = trace, trans = trans, trans_ind = trans_ind, trans_sum_ind = trans_sum_ind)
  return(dataList)
}
