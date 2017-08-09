#' A Markov Function
#'
#' This function computes the Markov trace and transition array for a given transition Matrix
#' @param M Transition probability matrix or array
#' @param p0 Initial state vector
#' @param n.cycles Total number of cycles to run the Markov model
#' @keywords Markov
#' @return trace A (n.cycles x n.states) matrix with Markov trace
#' @return trans A (n.states x n.states x n.cycles) array with transitions over time
#' CalculateMarkovTrace()
#'
CalculateMarkovTrace <- function(M, p0, n.cycles){
  # Extract number of states
  n.states <- ncol(M)
  # Extract name of states
  state.names <- colnames(M)
  # If no name of states, create default names
  if(is.null(state.names)){
    state.names <- paste("S", 1:n.states, sep = "_")
  }
  
  # Verify if M is 2D or 3D matrix
  n.dim <- length(dim(M))
  # If M is a 2D Matrix, convert to 3D by repeating it n.cycles times in a 3D array
  if (n.dim < 3){  
    M <- array(rep(M, n.cycles), 
               dim = c(n.states, n.states, n.cycles), 
               dimnames = list(state.names, state.names, 
                               paste("Cycle", 0:(n.cycles-1), sep = "")))
    
  }
  
  # Check if transition matrix is valid (i.e., each row should add up to 1)
  valid <- apply(M, 3, function(x) sum(rowSums(x))==n.states)
  #print(sum(valid))
  #print(n.cycles)
  
  if (!isTRUE(all.equal(as.numeric(sum(valid)), as.numeric(n.cycles)))) {
    print("Error: This is not a valid transition Matrix") 
    stop()
  }
  
  # Initialize trace matrix; Preallocate for speed for speed purposes
  trace <- matrix(0, ncol = n.states, nrow = (n.cycles)) 
  # Name trace's columns with states names taken from columns of M
  colnames(trace) <- state.names
  rownames(trace) <- paste("Cycle", 0:(n.cycles-1), sep = "")
  
  # Initialize trans array; Preallocate for speed for speed purposes
  trans <- array(0, 
                 dim = c(n.states, n.states, n.cycles), 
                 dimnames = list(state.names, state.names, 
                                 paste("Cycle", 0:(n.cycles-1), sep = "")))
  
  # First row of trace is initial state vector 
  trace[1, ]    <- p0
  # First row of first stack of trans array is initial state vector 
  diag(trans[, , 1]) <- p0
  
  # Run Markov model for n.cycles-1
  for (t in 2:n.cycles){
    trace[t, ]   <- trace[t-1, ] %*% M[, , t-1]
    trans[, , t] <- trace[t-1, ] * M[, , t-1]
  }
  # Return trace and trans
  return(list(trace = trace,  
              trans = trans)) 
}