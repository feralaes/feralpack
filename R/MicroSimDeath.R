
#' The MicroSimDeath function
#'
#' This function computes the microsimulation matrix keeping track of what happens to each individual at each cycle. While doing this the function includes age dependent mortality and has the opportunity to give the variable cumulative sick as output. This function is not universal and should only be used for the Sick-Sicker model example
#' @param a.TP Transition probability array  (n.s x n.s x n.t x n.i)
#' @param v.Init Initial state vector
#' @param n.i Number of hypothetical individuals simulated in the model
#' @param n.t Total number of cycles to run the model
#' @param seed Seed number for random number generator (optional)
#' @param cum Returns the cumulative variable that keeps track of the time spent as being sick (default = FALSE)
#' @keywords Microsimulation
#' @return m.Microsim A (n.i x n.t) matrix showing what happens to each individual at each cycle
#' @return cum A (n.i x n.t) matrix showing how long an individual has been ill
#' MicroSimDeath()
#'


MicroSimDeath <- function(a.TP, v.Init, n.i, n.t, seed = NULL, cum = FALSE) {

  # Set the seed for the random number generator
  set.seed(seed)
  # Define the number of health states
  n.s <- dim(a.TP)[1]
  states <- dimnames(a.TP)[1]

  # Define the set of states
  states <- seq(1, n.s)
  # Create a matrix for storing individuals' sequence
  sequence <- matrix(nrow = n.i, ncol = n.t)
  rownames(sequence) <- ind      # Name the rows
  colnames(sequence) <- cycles   # Name the columns

  # Create a matrix to keep track of time spend in S1 or S2
  cum_sick <- matrix(0, nrow = n.i, ncol = n.t)
  rownames(cum_sick) <- ind      # Name the rows
  colnames(cum_sick) <- cycles   # Name the columns

  for (i in 1:n.i) {
    # Choose the state for the first position in the sequence:
    first_state <- sample(states, 1, replace = TRUE, prob = v.Init)
    # Store the state for the first position of the sequence
    sequence[i, 1] <- first_state
    for (t in 2:n.t){
      prev_state <- sequence[i, t - 1] # Get the previous state in the new sequence
      # Get the probabilities of the current state, given previous state "prev_state":
      probabilities <- a.TP[prev_state, , t, i]
      # Choose the state at the current position of the sequence:
      state <- sample(states, 1, replace = TRUE, prob = probabilities)
      sequence[i, t] <- state  # Store the state for the current position of the sequence

      # cum_sick = 1 after being sick for a complete cycle t
      # The cumulative variable increases when someone stays in S1 or in S2 or goes from S2 to S3
      # Wjen the individual becomes healthy the variable is reset to 0
      if (state > 1 & prev_state > 1 & state != 4 &  prev_state != 4){
        cum_sick[i, t] <- cum_sick [i, t - 1] + 1
      } else {cum_sick[i, t] <- 0}

      # Correct the transition array in case someone has a history of being sick
      if (cum_sick[i, t] > 0 & t < n.t){
        a.TP [1, 4, , ] <- t(m.Die) * (1 + cum_sick[i, t] / 5) # Absolute increase of 20% (1/5) with every additional year of being sick
        a.TP [2, 4, t + 1, i] <- a.TP [1, 4, t + 1, i] * rr.S1  # Correct age specific mortality for beging sick
        a.TP [3, 4, t + 1, i] <- a.TP [1, 4, t + 1, i] * rr.S2  # correct age specific mortality for being sick

        # Calculate probabilities depending on other probabilities
        a.TP [1, 1, t + 1, i] <- 1 - a.TP [1, 2, t + 1, i] - a.TP [1, 4, t + 1, i]
        a.TP [2, 2, t + 1, i] <- 1 - a.TP [2, 1, t + 1, i] - a.TP [2, 3, t + 1, i] - a.TP [2, 4, t + 1, i]
        a.TP [3, 3, t + 1, i] <- 1 - a.TP [3, 4, t + 1, i]
      }
    }
  }
  if (cum == TRUE){ # If cum is TRUE, save the results in a list and return
    list <- list(progress = sequence, cum_sick = cum_sick)
    return(list)
  } else { # Otherwise only give the health state results
    return(sequence)
  }
}
