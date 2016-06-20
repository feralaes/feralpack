#' A Microsimulation function
#'
#' This function computes a microsimulation matrix that keeps track of what happens to each individual at each cycle
#' @param m.Transitions Transition probability matrix (2D, 3D or 4D)
#' @param p.0 Initial state vector
#' @param n.i Number of hypothetical individuals simulated in the model
#' @param n.t Total number of cycles to run the model
#' @param seed Seed number for random number generator (optional)
#' @keywords microsimulation
#' @return m.Microsim A (n.i x n.t) matrix showing state history of each individual at each cycle
#' MicroSim()
#'

# Works for both, 2-D, 3-D and 4-D matrixes
# size has to be in format (n.s x n.s x n.t x n.i)
MicroSim <- function(m.Transitions, p.0, n.i, n.t, seed = NULL) {

  # Set the seed for the random number generator
  set.seed(seed)

  # Check the dimensions of the transition matrix and convert to a 4D (n.s x n.s x n.t x n.i) matrix
  n.Dim <- length(dim(m.Transitions))
  if (n.Dim == 4){
    M <- m.Transitions
  }
  if (n.Dim == 3){ # It is a 3D Matrix, convert to 4D by repeating it n.i times
    M <- array (rep(m.Transitions, n.i),
      dim = c(n.s, n.s, n.t, n.i),
      dimnames = list(state_names, state_names, cycle_names, ind_names))
  }
  if (n.Dim == 2){ # It is a 2D Matrix, convert to 4D by repeating it n.t times
    L <- array(rep(m.Transitions, n.t),
      dim = c(n.s, n.s, n.t),
      dimnames = list(state_names, state_names, cycle_names))


    M <- array (rep(L, n.i), # Now it's a 3D Matrix, convert to 4D by repeating it n.i times
      dim = c(n.s, n.s, n.t, n.i),
      dimnames = list(state_names, state_names, cycle_names, ind_names))
  }
  # Define the number of health states
  n.s <- dim(M)[1]
  state_names <- dimnames(M)[1]

  # Define the set of states
  states <- seq(1, n.s)
  # Create a matrix for storing individuals' sequence
  sequence <- matrix(0, nrow = n.i, ncol = n.t)
  rownames(sequence) <- ind_names
  colnames(sequence) <- cycle_names

  for (i in 1:n.i) {
    # Choose the state for the first position in the sequence:
    first_state <- sample(states, 1, replace = TRUE, prob = p.0)
    # Store the state for the first position of the sequence
    sequence[i, 1] <- first_state
    for (t in 2:n.t) {
      prev_state <- sequence[i, t - 1] # Get the previous state in the new sequence
      # Get the probabilities of the current state, given previous state "prev_state":
      probabilities  <- M[prev_state, , t, i]
      # Choose the state at the current position of the sequence:
      state <- sample(states, 1, replace = TRUE, prob = probabilities)
      sequence[i, t]  <- state  # Store the state for the current position of the sequence
    }
  }
  return(sequence)
}
